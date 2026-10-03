import numpy as np

def int64_for_real(n):
    return np.arange(n)

def float32_for_real(n):
    return np.array([0.5, 1.5, 2.5][:n], dtype=np.float32)

def uint8_for_enum(n):
    return np.array([1, 0, 1][:n], dtype=np.uint8)

def bad_uint8_for_enum(n):
    return np.array([0, 2], dtype=np.uint8)

def matrix_for_pairs(n):
    return np.zeros((n, 2), dtype=np.int32)

def bytes_for_int32(n):
    return b"abc"

def bytes_for_uint8(n):
    return b"abc"

def bad_bytes_for_bool(n):
    return b"\x00\x01\x02"

def strided_for_real(n):
    return np.arange(10.0)[::2]

def swapped_for_real(n):
    return np.array([1.5, -2.25], dtype=">f8")
