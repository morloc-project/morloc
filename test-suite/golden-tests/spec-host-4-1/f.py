def scale(k, xs):
    return [k * x for x in xs]

def scale_r(k):
    return lambda xs: [k * x for x in xs]

def mk3(a, b, c):
    return a * 100 + b * 10 + c

def mk_r(a):
    return lambda b, c: 1000 + a * 100 + b * 10 + c
