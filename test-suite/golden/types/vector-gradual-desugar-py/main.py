import numpy as np


def mk_vec(n):
    """Produce a numpy uint8 vector of length n. The morloc-side type is
    `Vector U8` (gradual, no Nat annotation); the runtime shape is
    determined by n."""
    return np.zeros(n, dtype=np.uint8)


def mk_three():
    """Produce a length-3 numpy uint8 vector. The morloc-side type is
    `Vector 3 U8` (concrete). Runtime length must match the type
    annotation."""
    return np.array([1, 2, 3], dtype=np.uint8)
