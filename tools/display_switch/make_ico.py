"""Render display.svg at several sizes with Inkscape and pack them into display.ico (PNG-in-ICO)."""
import struct
import subprocess
import tempfile
from pathlib import Path

here = Path(__file__).parent
svg = here / "display.svg"
inkscape = r"C:\Program Files\Inkscape\bin\inkscape.exe"
sizes = [16, 24, 32, 48, 64, 128, 256]

pngs = []
with tempfile.TemporaryDirectory() as tmp:
    for s in sizes:
        out = Path(tmp) / f"display_{s}.png"
        subprocess.run([inkscape, str(svg), "--export-type=png", f"--export-filename={out}",
                        "-w", str(s), "-h", str(s), "--export-background-opacity=0"],
                       check=True, capture_output=True)
        pngs.append((s, out.read_bytes()))
    (here / "preview.png").write_bytes(pngs[-1][1])

header = struct.pack("<HHH", 0, 1, len(pngs))
offset = 6 + 16 * len(pngs)
entries, blobs = b"", b""
for s, data in pngs:
    dim = 0 if s >= 256 else s
    entries += struct.pack("<BBBBHHII", dim, dim, 0, 0, 1, 32, len(data), offset)
    blobs += data
    offset += len(data)
(here / "display.ico").write_bytes(header + entries + blobs)
print("wrote", here / "display.ico", "sizes", sizes)
