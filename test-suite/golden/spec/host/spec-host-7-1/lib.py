def pyadd(k, x):
    return k + x

def pycheck(k, x):
    if x == k:
        raise ValueError("pycheck rejects %d" % x)
    return x
