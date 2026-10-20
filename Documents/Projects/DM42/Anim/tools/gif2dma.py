"""Convert an animated GIF (e.g. from Jasc Animation Shop) into a DM42 animation file (.DMA).

    python gif2dma.py input.gif output.dma [--dither bayer|fs|threshold] [--fit fit|fill|stretch]
                      [--pad white|black] [--gamma 1.0] [--verify] [--sheet sheet.png]

File format (little-endian):
    header, 16 bytes:  'DMA1', u16 frames, u16 width (400), u16 height (240), u8 flags, 5 reserved
    each frame:        u8 delay (units of 10 ms), u8 n, then n records of [u8 line number][50 bytes]
Only the lines that differ from the previous frame are stored (the first frame stores all 240).
Line data is exactly what the DM42 LCD buffer holds: 50 bytes per line, running right to left
(display pixel x is bit i&7 of byte i>>3 with i = 399 - x), and a 0 bit is a black (inked) pixel.
"""
import argparse
import struct
import sys

import numpy as np
from PIL import Image, ImageSequence

W, H, LB = 400, 240, 50

BAYER8 = np.array([
    [0, 32, 8, 40, 2, 34, 10, 42], [48, 16, 56, 24, 50, 18, 58, 26],
    [12, 44, 4, 36, 14, 46, 6, 38], [60, 28, 52, 20, 62, 30, 54, 22],
    [3, 35, 11, 43, 1, 33, 9, 41], [51, 19, 59, 27, 49, 17, 57, 25],
    [15, 47, 7, 39, 13, 45, 5, 37], [63, 31, 55, 23, 61, 29, 53, 21]], dtype=np.float32)


def fit_frame(im, mode, pad):
    im = im.convert("RGBA")
    bg = Image.new("RGBA", im.size, (255, 255, 255, 255) if pad == "white" else (0, 0, 0, 255))
    im = Image.alpha_composite(bg, im).convert("L")
    if mode == "stretch":
        return im.resize((W, H), Image.LANCZOS)
    sx, sy = W / im.width, H / im.height
    s = max(sx, sy) if mode == "fill" else min(sx, sy)
    nw, nh = max(1, round(im.width * s)), max(1, round(im.height * s))
    im = im.resize((nw, nh), Image.LANCZOS)
    canvas = Image.new("L", (W, H), 255 if pad == "white" else 0)
    canvas.paste(im, ((W - nw) // 2, (H - nh) // 2))
    return canvas


def to_bits(gray, dither, gamma):
    g = np.asarray(gray, dtype=np.float32) / 255.0
    if gamma != 1.0:
        g = np.power(g, gamma)
    g *= 255.0
    if dither == "threshold":
        return g >= 128
    if dither == "bayer":
        thr = np.tile((BAYER8 + 0.5) * (255.0 / 64.0), (H // 8 + 1, W // 8 + 1))[:H, :W]
        return g > thr
    # Floyd-Steinberg (error diffusion): sharper stills, but shimmers and compresses worse in motion
    f = g.copy()
    out = np.zeros((H, W), dtype=bool)
    for y in range(H):
        for x in range(W):
            old = f[y, x]
            new = 255.0 if old >= 128 else 0.0
            out[y, x] = new > 0
            err = old - new
            if x + 1 < W: f[y, x + 1] += err * 7 / 16
            if y + 1 < H:
                if x > 0: f[y + 1, x - 1] += err * 3 / 16
                f[y + 1, x] += err * 5 / 16
                if x + 1 < W: f[y + 1, x + 1] += err * 1 / 16
    return out


def dilate(m, r=1):
    out = m.copy()
    for dy in range(-r, r + 1):
        for dx in range(-r, r + 1):
            out |= np.roll(np.roll(m, dy, 0), dx, 1)
    out[0, :] |= m[0, :]
    return out


def masked_fs(g, mask, base_bits):
    """Error diffusion confined to the pixels in `mask`; every other pixel keeps base_bits."""
    out = base_bits.copy()
    f = g.astype(np.float32).copy()
    ys, xs = np.nonzero(mask)
    if len(ys) == 0:
        return out
    for y in range(ys.min(), ys.max() + 1):
        for x in range(xs.min(), xs.max() + 1):
            if not mask[y, x]:
                continue
            old = f[y, x]
            new = 255.0 if old >= 128 else 0.0
            out[y, x] = new > 0
            err = old - new
            for yy, xx, w in ((y, x + 1, 7 / 16), (y + 1, x - 1, 3 / 16), (y + 1, x, 5 / 16), (y + 1, x + 1, 1 / 16)):
                if 0 <= yy < H and 0 <= xx < W and mask[yy, xx]:
                    f[yy, xx] += err * w
    return out


def stable_fs(grays, gamma, threshold):
    """Floyd-Steinberg quality without the shimmer: dither the static background (the median
    of all frames) once, and re-dither only the pixels that differ from it."""
    plate = np.median(np.stack([np.asarray(g, dtype=np.float32) for g in grays]), axis=0)
    plate_bits = to_bits(plate, "fs", gamma)

    def lin(a):
        a = np.asarray(a, dtype=np.float32) / 255.0
        return (np.power(a, gamma) if gamma != 1.0 else a) * 255.0

    pg = lin(plate)
    out = []
    for g in grays:
        gg = lin(g)
        mask = dilate(np.abs(gg - pg) > threshold, 1)
        out.append(masked_fs(gg, mask, plate_bits))
    return out


def pack(bits):
    """bits[y][x] True = white. LCD layout: the buffer runs right to left (the SDK's
    LCD_INVERT_XAXIS): display pixel x is bit (i & 7) of byte (i >> 3) with i = 399 - x;
    1 = white, 0 = black."""
    bits = bits[:, ::-1]
    b = bits.reshape(H, LB, 8).astype(np.uint8)
    weights = (1 << np.arange(8, dtype=np.uint8))
    return (b * weights).sum(axis=2).astype(np.uint8)          # (H, 50)


def encode(frames, delays):
    out = bytearray(b"DMA1" + struct.pack("<HHHB5x", len(frames), W, H, 1))
    prev = None
    sizes = []
    for fr, d in zip(frames, delays):
        if prev is None:
            changed = list(range(H))
        else:
            changed = [y for y in range(H) if not np.array_equal(fr[y], prev[y])]
        rec = bytearray([d, len(changed)])
        for y in changed:
            rec.append(y)
            rec += fr[y].tobytes()
        sizes.append(len(rec))
        out += rec
        prev = fr
    return bytes(out), sizes


def decode(data):
    assert data[:4] == b"DMA1"
    n, w, h, flags = struct.unpack_from("<HHHB", data, 4)
    assert (w, h) == (W, H)
    pos = 16
    cur = np.full((H, LB), 0xFF, dtype=np.uint8)
    frames, delays = [], []
    for _ in range(n):
        d, k = data[pos], data[pos + 1]
        pos += 2
        for _ in range(k):
            y = data[pos]
            cur[y] = np.frombuffer(data, dtype=np.uint8, count=LB, offset=pos + 1)
            pos += 1 + LB
        frames.append(cur.copy())
        delays.append(d)
    assert pos == len(data), "trailing bytes"
    return frames, delays


def unpack(fr):
    bits = np.zeros((H, W), dtype=bool)
    for x in range(W):
        i = W - 1 - x
        bits[:, x] = (fr[:, i >> 3] >> (i & 7)) & 1
    return bits


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("gif"); ap.add_argument("out")
    ap.add_argument("--dither", default="bayer", choices=["bayer", "fs", "fs-stable", "threshold"],
                    help="fs-stable: error diffusion for the moving parts over a once-dithered background")
    ap.add_argument("--plate-threshold", type=float, default=8.0, help="fs-stable: gray-level difference that counts as movement")
    ap.add_argument("--fit", default="fit", choices=["fit", "fill", "stretch"])
    ap.add_argument("--pad", default="white", choices=["white", "black"])
    ap.add_argument("--gamma", type=float, default=1.0)
    ap.add_argument("--verify", action="store_true")
    ap.add_argument("--sheet")
    a = ap.parse_args()

    src = Image.open(a.gif)
    grays, delays = [], []
    for i, f in enumerate(ImageSequence.Iterator(src)):
        grays.append(fit_frame(f.copy(), a.fit, a.pad))
        ms = f.info.get("duration", 100) or 100
        delays.append(max(1, min(255, round(ms / 10))))
    if a.dither == "fs-stable":
        frames = [pack(b) for b in stable_fs(grays, a.gamma, a.plate_threshold)]
    else:
        frames = [pack(to_bits(g, a.dither, a.gamma)) for g in grays]
    data, sizes = encode(frames, delays)
    open(a.out, "wb").write(data)

    n = len(frames)
    avg = sum(sizes) / n
    print(f"{n} frames, {len(data):,} bytes ({avg:,.0f} per frame; a raw frame is {H * LB:,}); "
          f"smallest {min(sizes)}, largest {max(sizes)}; frame delays {min(delays) * 10}-{max(delays) * 10} ms")
    if a.verify:
        dec, ddel = decode(data)
        assert len(dec) == n and ddel == delays, "frame count or delays differ"
        for i, (x, y) in enumerate(zip(frames, dec)):
            assert np.array_equal(x, y), f"frame {i} differs after decoding"
        print("verify: decoded file reproduces every frame exactly")
    if a.sheet:
        dec, _ = decode(data)
        pick = [dec[round(i * (n - 1) / 11)] for i in range(12)] if n > 12 else dec
        tiles = [Image.fromarray((unpack(f) * 255).astype(np.uint8)).resize((200, 120), Image.NEAREST) for f in pick]
        cols = 4
        rows = (len(tiles) + cols - 1) // cols
        sheet = Image.new("L", (cols * 204, rows * 124), 128)
        for i, t in enumerate(tiles):
            sheet.paste(t, ((i % cols) * 204 + 2, (i // cols) * 124 + 2))
        sheet.save(a.sheet)
        print("contact sheet written to", a.sheet)


if __name__ == "__main__":
    main()
