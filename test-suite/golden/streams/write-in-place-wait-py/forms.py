import numpy as np


def reals(kind, n):
    xs = [i * 0.5 - 3.25 for i in range(n)]
    return np.array(xs, dtype=np.float64) if kind == "np" else xs


def i32s(kind, n):
    xs = [(i * 7919) % 200003 - 100000 for i in range(n)]
    return np.array(xs, dtype=np.int32) if kind == "np" else xs


def i8s(kind, n):
    xs = [i % 256 - 128 for i in range(n)]
    return np.array(xs, dtype=np.int8) if kind == "np" else xs


def u8s(kind, n):
    xs = [(i * 31) % 256 for i in range(n)]
    if kind == "bytes":
        return bytes(xs)
    if kind == "bytearray":
        return bytearray(xs)
    return np.array(xs, dtype=np.uint8) if kind == "np" else xs


def bools(kind, n):
    xs = [i % 3 == 0 for i in range(n)]
    if kind == "bytes":
        return bytes(int(x) for x in xs)
    return np.array(xs, dtype=np.bool_) if kind == "np" else xs


def i16s(kind, n):
    """np8 is an int8 array, which numpy widens to I16."""
    xs = [i % 256 - 128 for i in range(n)]
    if kind == "np8":
        return np.array(xs, dtype=np.int8)
    return np.array(xs, dtype=np.int16) if kind == "np" else xs


def strs(kind, n):
    return [f"s{i}" + "y" * (i % 50) for i in range(n)]


def bad_bools(n, bad):
    """n bools as bytes; element `bad` (if in range) is the byte 2."""
    return bytes(2 if i == bad else i % 2 for i in range(n))
