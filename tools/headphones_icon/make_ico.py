"""Render headphones.svg at several sizes with Inkscape and pack them into headphones.ico (PNG-in-ICO)."""
import struct
import subprocess
from pathlib import Path

here = Path(__file__).parent
svg = here / "headphones.svg"
inkscape = r"C:\Program Files\Inkscape\bin\inkscape.exe"
sizes = [16, 24, 32, 48, 64, 128, 256]

pngs = []
for s in sizes:
    out = here / f"headphones_{s}.png"
    subprocess.run([inkscape, str(svg), "--export-type=png", f"--export-filename={out}",
                    "-w", str(s), "-h", str(s), "--export-background-opacity=0"],
                   check=True, capture_output=True)
    pngs.append((s, out.read_bytes()))

header = struct.pack("<HHH", 0, 1, len(pngs))
offset = 6 + 16 * len(pngs)
entries, blobs = b"", b""
for s, data in pngs:
    dim = 0 if s >= 256 else s
    entries += struct.pack("<BBBBHHII", dim, dim, 0, 0, 1, 32, len(data), offset)
    blobs += data
    offset += len(data)
(here / "headphones.ico").write_bytes(header + entries + blobs)
print("wrote", here / "headphones.ico", "sizes", sizes)
