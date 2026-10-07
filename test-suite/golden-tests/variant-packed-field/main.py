def pack_mymap(xs):
    return dict(xs)
def unpack_mymap(d):
    return list(d.items())
def size_map(d):
    assert isinstance(d, dict)
    return len(d)
def build(x):
    return {"a": x, "b": 2}
