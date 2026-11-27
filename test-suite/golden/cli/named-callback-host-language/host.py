def py_fold(f, init, xs):
    acc = init
    for x in xs:
        acc = f(acc, x)
    return acc


def py_tick(s):
    return None


def py_add(a, b):
    return a + b
