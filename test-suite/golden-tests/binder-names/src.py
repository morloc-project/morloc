def py_ap(f, x):
    return f(x)


def py_pt_a(p):
    return p["a"]


def py_flip(d):
    from mlc_types._local.main import Dir
    return Dir(1 - d.value)
