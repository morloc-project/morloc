def h_pack(x):
    return dict(x)
def h_unpack(x):
    return list(x.items())
def h_mapv(f, xs):
    return [(k, f(v)) for k, v in xs]
def h_inc(x):
    return x + 1
def h_size(xs):
    return len(xs)
