def pybad(n):
    return ["ok\n", b"a\xffb\n"]

def pyid(xs):
    return xs

def pyrender(xs):
    return b"a\xffb\n"

def pybadarray(n):
    import numpy as np
    return np.array(["ok\n", b"a\xffb\n"], dtype=object)

def pysurrogate(n):
    return ["ok\n", b"a\xffb\n".decode("utf-8", "surrogateescape")]
