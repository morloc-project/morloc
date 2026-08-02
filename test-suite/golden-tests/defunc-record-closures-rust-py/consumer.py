def use_ops(ops, x):
    return ops["inc"](x) + ops["scale"](x)


def use_list(fs, x):
    return fs[0](x) + fs[1](x)


def use_tuple(t, x):
    return t[0](x) + t[1](x)


def use_opt(f, x):
    return x if f is None else f(x)
