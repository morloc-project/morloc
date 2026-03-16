import numpy as np

def make1d():
    return np.arange(6, dtype=np.int32)

def make2d():
    return np.arange(6, dtype=np.int32).reshape(2, 3)

def make3d():
    return np.arange(24, dtype=np.int32).reshape(2, 3, 4)

def make4d():
    return np.arange(24, dtype=np.int32).reshape(2, 3, 2, 2)

def make5d():
    return np.arange(48, dtype=np.int32).reshape(2, 2, 2, 3, 2)
