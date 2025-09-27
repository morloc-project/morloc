def pneg(x):
    return (-1) * x

def padd(x, y):
    return x + y

def pmul(x, y):
    return x * y

def pmap(f, xs):
    return list(map(f, xs))

def pzipWith(f, xs, ys):
    return list(map(f, xs, ys))
