def h_pack(xs): return dict(xs)
def h_unpack(d): return list(d.items())
def h_size(d):
    assert isinstance(d, dict)
    return len(d)
