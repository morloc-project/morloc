def py_apply(f, x):
    return f(x)


def py_id(x):
    return x


def check(x, y, r):
    return r + (0 if x == y else 1)


def py_show_len(s):
    return len(s)


def py_mk(n):
    return {"m": n}


def py_m(o):
    return o["m"]
