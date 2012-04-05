
import sys
from random import random

def createngram(fname, length):
    ret = {}
    with open(fname) as fin:
        cont = fin.read()
    for f, t in zip(xrange(len(cont)), xrange(length, len(cont)+1)):
        key = cont[f:t]
        if key in ret:
            ret[key] += 1
        else:
            ret[key] = 1
    return ret
            

def getrandom(htb):
    lst = make_glist(htb)
    sm = sum(htb.itervalues())
    rnd = random() * sm
    return findelement(lst, rnd)

def make_glist(htb):
    ret = []
    x = 0
    for k, v in htb.iteritems():
        ret.append((x, x + v, k))
        x += v
    return ret
    
def findelement(lst, rnd):
    for l, h, ret in lst:
        if rnd >= l and rnd < h:
            return ret
    return lst[0][2]

def printhelp():
    print("""need 3 arguments : length, count and filename""")

def execute(length, count, filename):
    htb = createngram(filename, length)
    for x in xrange(count):
        sys.stdout.write(str(getrandom(htb)))

if __name__ == '__main__':
    ar = sys.argv
    if len(ar) != 4:
        printhelp()
    else:
        execute(int(sys.argv[1]), int(sys.argv[2]), sys.argv[3])
