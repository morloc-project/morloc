def fromNull(default_val, x):
    if x is None:
        return default_val
    return x

def addOpt(x, y):
    if x is None or y is None:
        return None
    return x + y

def identity(x):
    return x
