import numpy as np

# --- 2D Real (3x4, values 1..12) ---
def pyMakeMat():
    return np.arange(1, 13, dtype=np.float64).reshape(3, 4)

def pySumMat(m):
    return float(np.sum(m))

# --- 3D Real (2x3x4, values 0..23) ---
def pyMake3d():
    return np.arange(24, dtype=np.float64).reshape(2, 3, 4)

def pySum3d(t):
    return float(np.sum(t))

# --- 1D Real (10 elements, values 1..10) ---
def pyMakeVec():
    return np.arange(1, 11, dtype=np.float64)

def pySumVec(v):
    return float(np.sum(v))

# --- 4D Real (2x3x2x2, values 0..23) ---
def pyMake4d():
    return np.arange(24, dtype=np.float64).reshape(2, 3, 2, 2)

def pySum4d(t):
    return float(np.sum(t))

# --- 1D Int (8 elements, values 10..17) ---
def pyMakeIntVec():
    return np.arange(10, 18, dtype=np.int32)

def pySumIntVec(v):
    return int(np.sum(v))

# --- 1D Bool (6 elements: T,F,T,T,F,T) ---
def pyMakeBoolVec():
    return np.array([True, False, True, True, False, True])

def pyCountTrue(v):
    return int(np.sum(v))

# --- Empty tensor (0 elements) ---
def pyMakeEmpty():
    return np.array([], dtype=np.float64)

def pySumEmpty(v):
    return float(np.sum(v))

# --- Single element (value 42) ---
def pyMakeSingle():
    return np.array([42.0], dtype=np.float64)

def pySumSingle(v):
    return float(np.sum(v))

# --- Large tensor (5000 doubles, values 0..4999) ---
def pyMakeLarge():
    return np.arange(5000, dtype=np.float64)

def pySumLarge(v):
    return float(np.sum(v))

# --- Very large tensor (50000 doubles, crosses SHM threshold) ---
def pyMakeHuge():
    return np.arange(50000, dtype=np.float64)

def pySumHuge(v):
    return float(np.sum(v))
