import os, sys
sys.path.insert(0, os.path.dirname(__file__))
from docx_lib import Guide
import content_a as A
import content_b as B

g = Guide()
g.footer_numbers("Programming the SwissMicros DM42 — draft 1")
A.front(g)
for fn in (A.ch1, A.ch2, A.ch3, A.ch4, A.ch5, A.ch6, B.ch7, B.ch8, B.ch9, B.ch10, B.ch11, B.appendices):
    fn(g)
out = os.path.join(os.path.dirname(__file__), "DM42_Programming_Guide.docx")
g.save(out)
print("saved", out, os.path.getsize(out))
