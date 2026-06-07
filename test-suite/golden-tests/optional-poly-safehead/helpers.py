def list_is_empty(xs):
    return len(xs) == 0


def list_at(i, xs):
    return xs[i]


def unwrap_or(default, x):
    return default if x is None else x
