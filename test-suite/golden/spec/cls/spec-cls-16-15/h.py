def h_id(x):
    return x
def h_slice(a, b, c, xs):
    return xs[a:b:c]
def h_map_three(f, xs):
    assert len(xs) == 3, "three: got %d" % len(xs)
    return [f(x) for x in xs]
