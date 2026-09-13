def tick(n):
    return n * 10

def use_ops(ops, n):
    return ops["inc"](n) + ops["scale"](n)

def use_list(fs, n):
    return fs[0](n) + fs[1](n)
