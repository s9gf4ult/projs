def fac(a):pass

def fac(a):
    if a > 0:
        return reduce(lambda a, b: a*b , range(1, a+1))

print(str(fac(10)))
