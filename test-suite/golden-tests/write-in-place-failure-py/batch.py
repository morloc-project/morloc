def batch_of(b, n, bad):
    """n strings tagged with batch b; element `bad` (if in range) cannot be
    encoded as UTF-8 (a lone surrogate)."""
    return ["\ud800bad" if i == bad else f"{b}-{i}" + "x" * 40 for i in range(n)]


def count_tagged(b, xs):
    tag = f"{b}-"
    return sum(1 for x in xs if x.startswith(tag))


def reals_from_ints(big):
    """Python ints for a list of Real: the last one too large for a double
    when `big`."""
    return [2**64, 3, -(2**70)] + ([2**2000] if big else [])


def i8_batch(n, bad):
    """n small ints; element `bad` (if in range) does not fit an I8."""
    return [300 if i == bad else i % 100 for i in range(n)]


def bytes_of(b, n, bad):
    """n strings tagged with batch b, given as bytes; element `bad` (if in
    range) is not UTF-8."""
    return [b"a\xffb" if i == bad else (f"{b}-{i}" + "x" * 40).encode() for i in range(n)]


def arrays_of(kind):
    """A batch of I32 arrays: one-dimensional numpy arrays ("good"), or one
    of them two-dimensional ("2d") or a numpy scalar ("scalar")."""
    import numpy as np
    xs = [np.arange(k, dtype=np.int32) for k in range(5)]
    if kind == "2d":
        xs[2] = np.zeros((2, 2), dtype=np.int32)
    elif kind == "scalar":
        xs[2] = np.int32(7)
    return xs
