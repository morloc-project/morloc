def to_dict(xs):
    return {k: v for k, v in xs}

def from_dict(d):
    return list(d.items())

def keep(d):
    if not isinstance(d, dict):
        raise TypeError(f"expected a dict, got {type(d).__name__}")
    return d
