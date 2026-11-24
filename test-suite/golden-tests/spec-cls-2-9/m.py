class V:
    def __init__(self, xs):
        self.xs = xs

def pk(xs):
    return V(xs)

def un(v):
    return v.xs

def same(v):
    assert isinstance(v, V)
    return v
