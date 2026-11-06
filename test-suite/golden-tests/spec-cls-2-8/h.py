def h_shared(x):
    return tuple(x) if isinstance(x, list) else x
def h_unp(x): return list(x)
def h_len(x):
    assert isinstance(x, tuple)
    return len(x)
