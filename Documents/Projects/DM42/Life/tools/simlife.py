import random
W,H,GS,DB=200,120,27,25
def idx(y,j): return (y+1)*GS+1+j
def fix_edges(g):
    for r in range(1,H+1):
        g[r*GS]=g[r*GS+25]; g[r*GS+26]=g[r*GS+1]
    g[0:GS]=g[H*GS:(H+1)*GS]
    g[(H+1)*GS:(H+2)*GS]=g[GS:2*GS]
def west(cur,left): return ((cur<<1)|(left>>7))&255
def east(cur,right): return ((cur>>1)|(right<<7))&255
def step_asm(src):
    dst=[0]*len(src)
    for y in range(H):
        for j in range(DB):
            p=idx(y,j)
            def planes(a,al,ar): return [west(a,al),east(a,ar)]
            n0=src[p-GS]; n1=n2=0
            def addp(x):
                nonlocal n0,n1,n2
                t=n0&x; n0^=x; x=n1&t; n1^=t; n2|=x
            a,al,ar=src[p-GS],src[p-GS-1],src[p-GS+1]
            addp(west(a,al)); addp(east(a,ar))
            b,bl,br=src[p+GS],src[p+GS-1],src[p+GS+1]
            addp(b); addp(west(b,bl)); addp(east(b,br))
            m,ml,mr=src[p],src[p-1],src[p+1]
            addp(west(m,ml)); addp(east(m,mr))
            dst[p]=n1&(n0|m)&~n2&255
    fix_edges(dst); return dst
def to_cells(g): return [[(g[idx(y,x>>3)]>>(x&7))&1 for x in range(W)] for y in range(H)]
def step_ref(c):
    out=[[0]*W for _ in range(H)]
    for y in range(H):
        for x in range(W):
            n=sum(c[(y+dy)%H][(x+dx)%W] for dy in(-1,0,1) for dx in(-1,0,1))-c[y][x]
            out[y][x]=1 if n==3 or (n==2 and c[y][x]) else 0
    return out
random.seed(1)
g=[0]*((H+2)*GS)
for y in range(H):
    for j in range(DB): g[idx(y,j)]=random.getrandbits(8)&random.getrandbits(8) | (random.getrandbits(8)&random.getrandbits(8))
fix_edges(g)
c=to_cells(g)
for gen in range(40):
    g=step_asm(g); c=step_ref(c)
    assert to_cells(g)==c, f"mismatch gen {gen+1}"
print("40 generations match; live cells:",sum(map(sum,c)))
