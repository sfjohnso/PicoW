"""Make two synthetic test animations (400x240 GIFs) for the DM42 player.

bounce.gif  shaded ball over a gradient with drifting bands: a worst case for 1-bit dithering
cube.gif    rotating wireframe cube with a title: line art, which compresses very well
"""
import math
import os

import numpy as np
from PIL import Image, ImageDraw, ImageFont

W, H = 400, 240
out = os.path.join(os.path.dirname(__file__), "..", "samples")
os.makedirs(out, exist_ok=True)


def bounce(n=48):
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    frames = []
    for i in range(n):
        t = i / n
        base = 40 + 150 * (yy / H) + 35 * np.sin((xx / 28.0) + 2 * math.pi * t * 2)      # drifting bands
        bx = 200 + 130 * math.sin(2 * math.pi * t)
        by = 120 + 70 * abs(math.sin(2 * math.pi * t * 2)) * -1 + 60
        r = 46
        d = np.sqrt((xx - bx) ** 2 + (yy - by) ** 2)
        lit = np.clip(1 - d / r, 0, 1)
        shade = 255 * np.power(lit, 0.6) * np.clip(1 - ((xx - (bx - 14)) ** 2 + (yy - (by - 16)) ** 2) / (r * r * 1.7), 0.15, 1)
        img = np.where(d < r, np.maximum(shade, 25), base)
        shadow_d = np.sqrt(((xx - bx) / 1.0) ** 2 + ((yy - 212) / 0.22) ** 2)
        img = np.where((d >= r) & (shadow_d < r), img * 0.45, img)
        frames.append(Image.fromarray(np.clip(img, 0, 255).astype(np.uint8)).convert("RGB"))
    return frames


def cube(n=60):
    verts = [(x, y, z) for x in (-1, 1) for y in (-1, 1) for z in (-1, 1)]
    edges = [(a, b) for a in range(8) for b in range(a + 1, 8)
             if sum(abs(verts[a][k] - verts[b][k]) == 2 for k in range(3)) == 1]
    try:
        font = ImageFont.truetype("arial.ttf", 22)
    except OSError:
        font = ImageFont.load_default()
    frames = []
    for i in range(n):
        a = 2 * math.pi * i / n
        im = Image.new("RGB", (W, H), "white")
        dr = ImageDraw.Draw(im)
        pts = []
        for x, y, z in verts:
            x1, z1 = x * math.cos(a) + z * math.sin(a), -x * math.sin(a) + z * math.cos(a)
            y1, z2 = y * math.cos(a * 0.6) - z1 * math.sin(a * 0.6), y * math.sin(a * 0.6) + z1 * math.cos(a * 0.6)
            s = 90 / (3.5 - z2 * 0.6)
            pts.append((200 + x1 * s * 1.15, 120 + y1 * s * 1.15))
        for e0, e1 in edges:
            dr.line([pts[e0], pts[e1]], fill="black", width=3)
        dr.text((12, 8), "DM42", fill="black", font=font)
        dr.text((W - 12 - dr.textlength(f"frame {i + 1}/{n}", font=font), H - 32), f"frame {i + 1}/{n}", fill="black", font=font)
        frames.append(im)
    return frames


def ball_static(n=48):
    """Static gradient + texture background; only a small ball moves."""
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    base = 70 + 130 * (yy / H) + 25 * np.sin(xx / 17.0) * np.sin(yy / 23.0)
    frames = []
    for i in range(n):
        t = i / n
        bx = 200 + 150 * math.sin(2 * math.pi * t)
        by = 120 + 50 * math.sin(2 * math.pi * t * 3)
        r = 26
        d = np.sqrt((xx - bx) ** 2 + (yy - by) ** 2)
        lit = np.clip(1 - d / r, 0, 1)
        ball = 40 + 215 * np.power(lit, 0.7)
        img = np.where(d < r, ball, base)
        frames.append(Image.fromarray(np.clip(img, 0, 255).astype(np.uint8)).convert("RGB"))
    return frames


for name, frames, ms in (("bounce", bounce(), 40), ("cube", cube(), 40), ("ballstat", ball_static(), 40)):
    path = os.path.join(out, name + ".gif")
    frames[0].save(path, save_all=True, append_images=frames[1:], duration=ms, loop=0, optimize=False)
    print(path, len(frames), "frames")
