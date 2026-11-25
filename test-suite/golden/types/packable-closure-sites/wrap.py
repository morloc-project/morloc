class WrapPy:
    def __init__(self, v):
        self.v = v

def pack_wrap(x):
    return WrapPy(x)

def unpack_wrap(w):
    return w.v

def key_py(w):
    return w.v

def py_apply(f, x):
    return f(x)

def py_apply_w(f, w):
    return f(w)

def py_mk(x):
    return WrapPy(x)

def py_return(f, x):
    return f(x)
