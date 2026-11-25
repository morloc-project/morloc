def identity(x):
    return x

def fromNull(default_val, x):
    if x is None:
        return default_val
    return x

def isNull(x):
    return x is None
