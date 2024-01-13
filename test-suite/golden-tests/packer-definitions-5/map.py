def packMap(xs):
    return dict(zip(xs[0], xs[1]))

def unpackMap(d):
    return (list(d.keys()), list(d.values()))

def singleton(k, v):
    return {k : v}
