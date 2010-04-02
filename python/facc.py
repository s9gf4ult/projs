def fac(a):pass

def fac(a):
    if a <= 1:
        return 1
    else:
        return fac(a - 1) * a


print(str(fac(10)))
