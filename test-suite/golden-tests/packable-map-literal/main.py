def pack_mymap(xs):
    """Wire (list of (k, v) tuples) -> Native (dict)."""
    return dict(xs)


def unpack_mymap(d):
    """Native (dict) -> Wire (list of (k, v) tuples)."""
    return list(d.items())


def size_map(d):
    return len(d)
