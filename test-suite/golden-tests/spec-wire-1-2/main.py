def freeze(xs):
    return tuple(xs)

def thaw(t):
    return list(t)

def keep(t):
    if not isinstance(t, tuple):
        raise TypeError(f"expected a tuple, got {type(t).__name__}")
    return t
