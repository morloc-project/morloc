import os

def tick(path):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)

def mark(s):
    return s

def ident(x):
    return x

def take_thunk(t):
    t()
    t()
    return t()

def use_record(h):
    h["run"]()
    h["run"]()
    return h["run"]()

def use_list(xs):
    xs[0]()
    xs[0]()
    return xs[0]()

def use_callback(f, x):
    f(x)
    f(x)
    return f(x)

def use_pinned(c, x):
    c["inc"](x)
    c["inc"](x)
    return c["inc"](x)
