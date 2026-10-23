def freeze(xs):
    return tuple(xs)

def keep(t):
    if not isinstance(t, tuple):
        raise TypeError(f"expected a tuple, got {type(t).__name__}")
    return len(t)
