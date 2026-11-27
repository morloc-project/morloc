def py_double(x):
    return 2 * x

def py_tick(k):
    return 100 + k

def py_use_ops(ops, n):
    return ops["inc"](n) + ops["total"]()
