import ctypes


# A real fault, not a raised signal: a signal sent with kill() is
# process-directed and may land on a thread other than the one running the
# manifold, which then returns its result before the process dies.
def crash(x):
    ctypes.c_long.from_address(0).value = x
    return x
