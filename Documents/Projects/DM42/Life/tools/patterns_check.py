import re
from collections import Counter
PAT = {
 'glider':        "bo$2bo$3o!",
 'lwss':          "bo2bo$o$o3bo$4o!",
 'rpentomino':    "b2o$2ob$bo!",
 'acorn':         "bo$3bo$2o2b3o!",
 'diehard':       "6bo$2o$bo3b3o!",
 'pentadecathlon':"2bo4bo$2ob4ob2o$2bo4bo!",
 'pulsar':        "2b3o3b3o2$o4bobo4bo$o4bobo4bo$o4bobo4bo$2b3o3b3o2$2b3o3b3o$o4bobo4bo$o4bobo4bo$o4bobo4bo2$2b3o3b3o!",
 'gosper':        "24bo$22bobo$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o$2o8bo3bob2o4bobo$10bo5bo7bo$11bo3bo$12b2o!",
}
def parse(s):
    cells=[]; run=0; x=y=0
    for ch in s:
        if ch.isdigit(): run=run*10+int(ch); continue
        n=run or 1; run=0
        if ch=='b': x+=n
        elif ch=='o': cells+=[(x+k,y) for k in range(n)]; x+=n
        elif ch=='$': y+=n; x=0
        elif ch=='!': break
    return set(cells)
def step(live):
    c=Counter((x+dx,y+dy) for x,y in live for dx in(-1,0,1) for dy in(-1,0,1) if dx or dy)
    return {p for p,n in c.items() if n==3 or (n==2 and p in live)}
def norm(s):
    mx=min(x for x,y in s); my=min(y for x,y in s)
    return frozenset((x-mx,y-my) for x,y in s)
def run(name,gens):
    s=parse(PAT[name]); hist=[len(s)]
    for _ in range(gens): s=step(s); hist.append(len(s))
    return s,hist
for n in PAT: print(f"{n:15s} cells={len(parse(PAT[n]))}")
# glider: translates by (1,1) every 4 gens
g=parse(PAT['glider']); s=g
for _ in range(4): s=step(s)
assert norm(s)==norm(g) and len(s)==5; print("glider ok (period 4, 5 cells)")
# lwss: 9 cells, returns shifted by 2 after 4 gens
g=parse(PAT['lwss']); s=g
for _ in range(4): s=step(s)
assert len(g)==9 and norm(s)==norm(g) and s!=g; print("lwss ok (9 cells, translates)")
# pulsar: period 3, 48 cells
g=parse(PAT['pulsar']); s=g
for _ in range(3): s=step(s)
assert len(g)==48 and s==g and step(g)!=g; print("pulsar ok (48 cells, period 3)")
# pentadecathlon: 12 cells, period 15
g=parse(PAT['pentadecathlon']); s=g
for i in range(15):
    s=step(s)
    if i<14: assert s!=g
assert s==g and len(g)==12; print("pentadecathlon ok (12 cells, period 15)")
# diehard: dies out at gen 130
s,h=run('diehard',140); assert h[130]==0 and h[129]!=0; print("diehard ok (extinct at gen 130)")
# r-pentomino: 5 cells; settles at pop 116 by gen 1103 (open plane)
s,h=run('rpentomino',1200); assert len(parse(PAT['rpentomino']))==5 and h[1103]==116 and h[1200]==116; print("r-pentomino ok (pop 116 at gen 1103)")
# acorn: 7 cells; pop 633 at gen 5206
s,h=run('acorn',5300); assert len(parse(PAT['acorn']))==7 and h[5206]==633; print("acorn ok (pop 633 at gen 5206)")
# gosper gun: 36 cells; period 30, emits a glider (+5 cells) each period
g=parse(PAT['gosper']); assert len(g)==36
s=g; pops=[36]
for i in range(1,91):
    s=step(s); pops.append(len(s))
assert pops[30]==41 and pops[60]==46 and pops[90]==51, (pops[30],pops[60],pops[90]); print("gosper gun ok (36 cells, +5 per 30 gens)")
print("ALL PATTERNS VERIFIED")
