
import sys
from random import random

def createngram(fname, length):
    ret = {}
    cont = u''
    with open(fname) as fin:
        for ln in fin.readlines():
            cont += ln
    for f, t in zip(xrange(len(cont)), xrange(length, len(cont)+1)):
        key = cont[f:t]
        if key in ret:
            ret[key] += 1
        else:
            ret[key] = 1
    return ret
            
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
    print('ngram builded {0} elements'.format(len(htb)))
    lst = make_glist(htb)
    sm = sum(htb.itervalues())
    for x in xrange(count):
        rnd = random() * sm
        sys.stdout.write(findelement(lst, rnd))

if __name__ == '__main__':
    ar = sys.argv
    reload(sys)
    sys.setdefaultencoding('utf-8')
    if len(ar) != 4:
        printhelp()
    else:
        execute(int(sys.argv[1]), int(sys.argv[2]), sys.argv[3])
