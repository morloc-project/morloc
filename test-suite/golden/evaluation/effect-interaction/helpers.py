_state = {"count": 0}


def py_read_opt(x):
    """Returns None if x < 0, else x. Wire: ?Int as int|None."""
    if x < 0:
        return None
    return x


def py_add_one(x):
    return x + 1


def py_maybe_error(x):
    """Would fail on x < 0; on positive x just doubles."""
    if x < 0:
        raise RuntimeError(f"py_maybe_error: negative x={x}")
    return x * 2


def py_counter():
    _state["count"] += 1
    return _state["count"]


def py_reset():
    _state["count"] = 0
    return None


def py_apply_thunk(f):
    """Take a nullary callable, force it, return value + 1."""
    return f() + 1


def py_sum_thunks(xs):
    """Iterate a list of nullary callables, force each, sum."""
    return sum(f() for f in xs)


def py_consume_tuple(t):
    """Two-slot tuple of nullary callables; force both, sum."""
    a, b = t
    return a() + b()


