
points = [(1,1),(3,4),(-1,0)]

def genPaths(a, b) -> int:
    (ax, ay) = a
    (bx, by) = b
    hside = bx - ax
    vside = by - ay
    if hside == 0 and vside == 0:
        return 0
    if hside == 0:
        return abs(vside)
    if vside == 0:
        return abs(hside)
    hplus = 1 if hside > 0 else -1
    vplus = 1 if vside > 0 else -1
    a1 = (ax + hplus, ay)
    a2 = (ax, ay + vplus)
    a3 = (ax + hplus, ay + vplus)
    pths = [genPaths(a1, b), genPaths(a2, b)] #, genPaths(a3, b)]
    return(min(pths)+1)

def yoba(p):
    ret = 0
    for i in range(0, len(p) - 1):
        ret += genPaths(p[i], p[i+1])
    return ret
