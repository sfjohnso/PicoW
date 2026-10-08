import subprocess, sys
sys.path.insert(0,'.')
from patterns_check import PAT, parse
W,H=200,120
out=subprocess.run(['./host_test.exe'],capture_output=True,text=True).stdout.splitlines()
res={}
for l in out:
    if ' o' in l.split(':')[0] and ':' in l:
        k,v=l.split(':',1); res[k]={tuple(map(int,t.split(','))) for t in v.split()}
    elif l.startswith('clear_rect'): print(l)
    elif ' size ' in l: res[l.split()[0]+' size']=tuple(map(int,l.split()[2:]))
names=["glider","lwss","rpentomino","acorn","diehard","pentadecathlon","pulsar","gosper"]
ok=True
for n in names:
    cells=parse(PAT[n]); w=max(x for x,y in cells)+1; h=max(y for x,y in cells)+1
    if res[n+' size']!=(w,h): ok=False; print("SIZE MISMATCH",n)
    for o in range(8):
        ww,hh,cs=w,h,cells
        if o&4: cs={(y,x) for x,y in cs}; ww,hh=hh,ww
        if o&1: cs={(ww-1-x,y) for x,y in cs}
        if o&2: cs={(x,hh-1-y) for x,y in cs}
        exp={((197+x)%W,(118+y)%H) for x,y in cs}
        if res[f"{n} o{o}"]!=exp: ok=False; print("MISMATCH",n,o)
print("stamp matches Python for all 8 patterns x 8 orientations" if ok else "FAILED")
