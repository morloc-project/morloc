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
