"""Prepare figures for the DM42 programming guide (photos, screenshots, charts)."""
import os
from PIL import Image, ImageDraw, ImageFont

ROOT = r"C:\Users\Steve\Documents\Projects\DM42"
OUT = os.path.join(ROOT, "Guide", "fig")
DL = r"C:\Users\Steve\Downloads"

def font(sz, bold=False):
    for n in (("arialbd.ttf" if bold else "arial.ttf"),):
        try: return ImageFont.truetype(n, sz)
        except OSError: pass
    return ImageFont.load_default()

# photos: 8268 / 8269 were shot sideways
for src, dst, rot in (("IMG_8268.jpg", "photo_table.png", 90), ("IMG_8269.jpg", "photo_setup.png", 90), ("IMG_8270.jpg", "photo_keyboard.png", 0)):
    im = Image.open(os.path.join(DL, src)).convert("RGB")
    if rot: im = im.rotate(rot, expand=True)
    im.save(os.path.join(OUT, dst))

# screenshots (800x480): copy with a thin frame
def shot(rel, name):
    im = Image.open(os.path.join(ROOT, rel)).convert("RGB")
    fr = Image.new("RGB", (im.width + 4, im.height + 4), (60, 60, 60)); fr.paste(im, (2, 2))
    fr.save(os.path.join(OUT, name))
shots = {
 "ack_setup.png": r"Ackermann\screens\20261013-16442221.png",   # polished setup screen m=4 n=1 (index 7 on sheet)
}
import glob
A = sorted(glob.glob(os.path.join(ROOT, "Ackermann", "screens", "*.png")))
Z = sorted(glob.glob(os.path.join(ROOT, "ZMachine", "screens", "*.png")))
AN = sorted(glob.glob(os.path.join(ROOT, "Anim", "screens", "*.png")))
names = {  # index in A / Z / AN lists (see contact sheet)
 "ack_menu_early.png": A[0], "ack_result_small.png": A[1], "ack_table.png": A[2], "ack_run_a.png": A[3],
 "ack_run_b.png": A[4], "ack_brute_310.png": A[5], "ack_memo_310.png": A[6], "ack_setup.png": A[7],
 "ack_run41_0.png": A[8], "ack_run41_46.png": Z[0], "ack_run41_91.png": Z[1], "ack_done41.png": Z[2],
 "disk_usb1.png": AN[0], "disk_usb2.png": AN[1], "disk_bat1.png": AN[2], "disk_bat2.png": AN[3],
}
for n, p in names.items():
    im = Image.open(p).convert("RGB")
    fr = Image.new("RGB", (im.width + 4, im.height + 4), (60, 60, 60)); fr.paste(im, (2, 2)); fr.save(os.path.join(OUT, n))

# animation figures
sam = os.path.join(ROOT, "Anim", "samples")
a = Image.open(os.path.join(sam, "ballstat_changes.png")).convert("RGB"); b = Image.open(os.path.join(sam, "ballstat_stable_changes.png")).convert("RGB")
comp = Image.new("RGB", (400 * 3 + 20, 240 + 34), (128, 128, 128)); dd = ImageDraw.Draw(comp)
comp.paste(a.crop((0, 0, 400, 240)), (0, 34)); comp.paste(a.crop((410, 0, 810, 240)), (410, 34)); comp.paste(b, (820, 34))
for x, t in ((8, "Ordered (Bayer)"), (418, "Naive error diffusion"), (828, "Stable error diffusion")): dd.text((x, 6), t, fill="white", font=font(20, True))
comp.save(os.path.join(OUT, "anim_changes_all.png"))
for src, dst in (("ballstat_changes.png", "anim_changes_ordered_naive.png"), ("ballstat_stable_changes.png", "anim_changes_stable.png"),
                 ("coriolis/sheet_final.png", "anim_coriolis_sheet.png"), ("cube_sheet.png", "anim_cube_sheet.png"), ("bounce_sheet.png", "anim_bounce_sheet.png")):
    Image.open(os.path.join(sam, src)).convert("RGB").save(os.path.join(OUT, dst))

# bar charts ------------------------------------------------------------
def bars(path, title, groups, series, unit, colors, w=1400, h=700, logscale=False):
    im = Image.new("RGB", (w, h), "white"); d = ImageDraw.Draw(im)
    d.text((w // 2, 18), title, fill="black", font=font(34, True), anchor="ma")
    L, R, T, B = 140, w - 40, 110, h - 150
    mx = max(v for g in groups for v in g[1])
    mx = ((mx // 5000) + 1) * 5000 if mx > 5000 else (int(mx / 10) + 1) * 10
    for i in range(6):
        y = B - (B - T) * i / 5
        d.line([(L, y), (R, y)], fill=(215, 215, 215), width=2)
        d.text((L - 12, y), f"{mx * i / 5:,.0f}", fill="black", font=font(24), anchor="rm")
    d.text((L, T - 22), unit, fill="black", font=font(26), anchor="ls")
    gw = (R - L) / len(groups); bw = gw / (len(series) + 1)
    for gi, (label, vals) in enumerate(groups):
        x0 = L + gi * gw + bw / 2
        for si, v in enumerate(vals):
            x = x0 + si * bw; y = B - (B - T) * v / mx
            d.rectangle([x, y, x + bw * 0.9, B], fill=colors[si])
            d.text((x + bw * 0.45, y - 6), f"{v:,.1f}".rstrip("0").rstrip(".") if v < 100 else f"{v:,.0f}", fill="black", font=font(24), anchor="ms")
        d.text((L + gi * gw + gw / 2, B + 14), label, fill="black", font=font(26), anchor="ma")
    for si, s in enumerate(series):
        x = L + si * 420; d.rectangle([x, h - 62, x + 30, h - 32], fill=colors[si]); d.text((x + 40, h - 47), s, fill="black", font=font(26), anchor="lm")
    im.save(path)

C1, C2, C3 = (31, 90, 160), (214, 110, 30), (80, 150, 80)
bars(os.path.join(OUT, "chart_disk.png"), "FAT drive read speed (1 MiB test file, two runs each)",
     [("512 B reads", (11151, 2124)), ("4 KB reads", (16000, 2547)), ("12,000 B frame", (15893, 2472)), ("32 KB reads", (17676, 2585)), ("seek + 12,000 B", (10906, 1883))],
     ("USB power, 80 MHz", "Battery, 24 MHz"), "KB/s", (C1, C2))
bars(os.path.join(OUT, "chart_life.png"), "Life: generations per second, USB power",
     [("Soup", (40.9,)), ("Garden", (52.4,)), ("Gun battle", (73.8,)), ("Hybrid", (42.3,)), ("Kaleido-\nscope", (44.1,)), ("Full redraw\n(first version)", (36.5,))],
     ("generations/s",), "gens/s", (C1,))
bars(os.path.join(OUT, "chart_anim_frame.png"), "Where a full-screen frame's time goes (ANIM1, 240 lines/frame)",
     [("USB power", (0.0, 1.0, 0.0)), ("Battery", (5.4, 1.9, 68.9))],
     ("read file", "copy to buffer", "wait for LCD transfer"), "ms per frame", (C1, C3, C2), w=1400, h=700)
bars(os.path.join(OUT, "chart_dither.png"), "Pixels that change per frame, moving ball on a still background",
     [("Ordered (Bayer)", (597,)), ("Stable error diffusion", (1653,)), ("Naive error diffusion", (14683,))],
     ("pixels changed per frame",), "pixels", (C1,))
print(sorted(os.listdir(OUT)))
