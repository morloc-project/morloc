def apply_fn(f, n):
    return f(n)

def use_ops(ops, n):
    return ops["inc"](n) + ops["same"](n)

def tick(n):
    return n * 10
