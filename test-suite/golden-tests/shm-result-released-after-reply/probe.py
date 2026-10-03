import pymorloc as morloc


def heldAfter(xs):
    del xs
    morloc.shm_tracker_flush()
    return morloc.shm_live_bytes()


def big_py(n):
    return [7] * n
