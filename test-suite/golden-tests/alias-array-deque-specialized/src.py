import numpy as np

# Array variants -- morloc Array on the Python side serializes via List;
# return numpy arrays so the numpy fast-path is exercised, mirroring how
# real users would supply specialized integer arrays.

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

# Deque variants -- same payloads, exercised through the Deque alias path

def pyMakeI32D():
    return np.arange(10, 18, dtype=np.int32)

def pySumI32D(v):
    return int(np.sum(np.asarray(v, dtype=np.int32)))

def pyMakeF32D():
    return np.array([1.5, 2.5, 3.5], dtype=np.float32)

def pySumF32D(v):
    return float(np.sum(np.asarray(v, dtype=np.float32)))
