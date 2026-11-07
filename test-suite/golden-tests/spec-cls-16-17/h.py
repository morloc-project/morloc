def h_id(x):
    return x
def h_slice(a, b, c, xs):
    return xs[a:b:c]
def h_rev(xs):
    return list(reversed(xs))
def h_map(f, xs):
    return [f(x) for x in xs]
def h_rev_map(f, xs):
    return [f(x) for x in reversed(xs)]
