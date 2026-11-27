import numpy as np


def mk_mat(m, n):
    # Deterministic 1..m*n reshaped to (m, n) as float32 to match Matrix F32.
    return np.arange(1, m * n + 1, dtype=np.float32).reshape(m, n)


def mat_sum(m):
    return float(np.sum(m))
