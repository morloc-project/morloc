def scale(k):
    def go(xs):
        return [k * x for x in xs]
    return go

def ident(x):
    return x
