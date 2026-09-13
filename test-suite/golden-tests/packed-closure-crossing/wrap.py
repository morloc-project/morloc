def pack_wrap(x):
    return x

def unpack_wrap(w):
    return w

def tick():
    return 0

def add_wrapped(a, w):
    return a + w

def apply_wrapped_py(f, w):
    return f(w)

def mk_wrapped_py(x):
    return x

def mk_wrapped_from(a, x):
    return a + x

def use_ops(ops, w):
    return ops["f"](w)
