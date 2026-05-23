import numpy as np

# ---------------------------------------------------------------------------
# Round-trip baselines.
# ---------------------------------------------------------------------------
def pyMakeMat34():
    return np.arange(1, 13, dtype=np.float64).reshape(3, 4)

def pySumMat34(m):
    return float(np.sum(m))

def pyMakeT3():
    return np.arange(24, dtype=np.float64).reshape(2, 3, 4)

def pySumT3(t):
    return float(np.sum(t))

def pyMakeT4():
    return np.arange(24, dtype=np.float64).reshape(2, 3, 2, 2)

def pySumT4(t):
    return float(np.sum(t))

def pyMakeT5():
    return np.arange(32, dtype=np.float64).reshape(2, 2, 2, 2, 2)

def pySumT5(t):
    return float(np.sum(t))

# ---------------------------------------------------------------------------
# matmul inputs and consumer.
# ---------------------------------------------------------------------------
def pyMakeA23():
    return np.arange(1, 7, dtype=np.float64).reshape(2, 3)

def pyMakeB32():
    return np.arange(1, 7, dtype=np.float64).reshape(3, 2)

def pySumMat22(m):
    return float(np.sum(m))

# ---------------------------------------------------------------------------
# Corner extraction probe. Indices use the semantic (row-major) view.
# numpy is row-major by default, so t[i,j,k] matches Cpp t(i,j,k).
# Expected: [0, 12, 11, 23] for a Tensor3 2x3x4 with values 0..23.
# ---------------------------------------------------------------------------
def pyCornersT3(t):
    return [float(t[0,0,0]), float(t[1,0,0]), float(t[0,2,3]), float(t[1,2,3])]

# ---------------------------------------------------------------------------
# Element-typed matrices (4x4, values 1..16, sum = 136).
# ---------------------------------------------------------------------------
def pyMakeMatI32():
    return np.arange(1, 17, dtype=np.int32).reshape(4, 4)

def pySumMatI32(m):
    return int(np.sum(m.astype(np.int64)))

def pyMakeMatI64():
    return np.arange(1, 17, dtype=np.int64).reshape(4, 4)

def pySumMatI64(m):
    return int(sum(int(x) for x in m.reshape(-1).tolist()))

def pyMakeMatF32():
    return np.arange(1, 17, dtype=np.float32).reshape(4, 4)

def pySumMatF32(m):
    return float(np.sum(m))

# ---------------------------------------------------------------------------
# Zero-copy probes. The user-facing ndarray for ranks >= 2 is the result
# of reshape(), which always returns a view: DIRECT (its own OWNDATA) is
# always False. The witness for SHM zero-copy is the OWNDATA of the ROOT
# of the .base chain. If ROOT=False, the buffer is external (SHM-backed
# numpy view); if ROOT=True, somewhere up the chain a numpy-owned buffer
# was allocated (a copy happened).
# ---------------------------------------------------------------------------
def _ownroot(arr):
    while hasattr(arr, 'base') and arr.base is not None:
        arr = arr.base
    if hasattr(arr, 'flags'):
        return bool(arr.flags['OWNDATA'])
    return None

def _report(arr):
    direct = bool(arr.flags['OWNDATA']) if hasattr(arr, 'flags') else None
    root = _ownroot(arr)
    size = int(arr.size if hasattr(arr, 'size') else 0)
    return "DIRECT=%s,ROOT=%s,size=%d" % (str(direct), str(root), size)

def pyOwnDataMat(m):
    return _report(m)

def pyOwnDataT3(t):
    return _report(t)

def pyOwnDataT5(t):
    return _report(t)

# ---------------------------------------------------------------------------
# Edge cases.
# ---------------------------------------------------------------------------
def pyMakeUnitMat():
    return np.array([[7.0]], dtype=np.float64)

def pySumUnitMat(m):
    return float(np.sum(m))

def pyMakeUnitT3():
    return np.array([[[9.0]]], dtype=np.float64)

def pySumUnitT3(t):
    return float(np.sum(t))

# ---------------------------------------------------------------------------
# Polymorphic-in-shape sum helpers for literal-input tests. The morloc
# Matrix m n a / Tensor3 d1 d2 d3 a maps to numpy.ndarray regardless of
# specific dims, so these accept any shape.
# ---------------------------------------------------------------------------
def pySumMatPoly(m):
    return float(np.sum(m))

def pySumMatI32Poly(m):
    return int(np.sum(m.astype(np.int64) if hasattr(m, 'astype') else m))

def pySumT3Poly(t):
    return float(np.sum(t))
