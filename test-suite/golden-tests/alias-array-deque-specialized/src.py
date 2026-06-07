import numpy as np

# Deque variants. Python's Deque maps to "list" at the wire level
# (Packable (List a) (Deque a) is the identity), so returning numpy
# arrays / lists works the same way as for plain List.

def pyMakeI32():
    return np.arange(10, 18, dtype=np.int32)

def pySumI32(v):
    return int(np.sum(np.asarray(v, dtype=np.int32)))

def pyMakeI64():
    return np.arange(10, 18, dtype=np.int64)

def pySumI64(v):
    return int(np.sum(np.asarray(v, dtype=np.int64)))

def pyMakeF32():
    return np.array([1.5, 2.5, 3.5], dtype=np.float32)

def pySumF32(v):
    return float(np.sum(np.asarray(v, dtype=np.float32)))

def pyMakeF64():
    return np.array([1.5, 2.5, 3.5], dtype=np.float64)

def pySumF64(v):
    return float(np.sum(np.asarray(v, dtype=np.float64)))
