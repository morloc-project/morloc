import numpy as np

def make_vec(n):
    return np.array([float(n), float(n + 1), float(n + 2)])

def list_sum(xs):
    if not isinstance(xs, list):
        raise TypeError(f"expected a list, got {type(xs).__name__}")
    return float(sum(xs))
