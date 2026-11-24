def scaler(k):
    return (lambda v: [k * x for x in v], k)

def fst(p):
    return p[0]

def on_range(k, f):
    return f(list(range(k)))

def total(v):
    return sum(v)

def range_k(k):
    return list(range(k))
