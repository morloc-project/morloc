class MyIntClass:
    def __init__(self, v):
        self.v = v


def pack_myint(x):
    """Wire (int) -> Native (MyIntClass): construct a wrapper."""
    return MyIntClass(x)


def unpack_myint(x):
    """Native (MyIntClass) -> Wire (int): extract the primitive."""
    return x.v


def to_int(x):
    return x.v


class MyVecClass:
    def __init__(self, xs):
        self.xs = xs


def pack_myvec(xs):
    """Wire (list) -> Native (MyVecClass): construct a wrapper."""
    return MyVecClass(xs)


def unpack_myvec(x):
    """Native (MyVecClass) -> Wire (list): extract the underlying list."""
    return x.xs


def len_vec(v):
    return len(v.xs)
