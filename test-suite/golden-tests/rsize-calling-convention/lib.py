def scale(factor):
    return lambda xs: [factor * x for x in xs]


def shift(offset, xs):
    return [offset + x for x in xs]


def between(lo):
    return lambda hi: lambda x: lo <= x and x <= hi


def clamp_lo_hi(lo, hi):
    return lambda x: min(max(x, lo), hi)


def native_name_add(a, b):
    return a + b
