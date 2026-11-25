def toNull(x):
    return x

def isNull(x):
    return x is None

def fromNull(default_val, x):
    if x is None:
        return default_val
    return x

def doubleOptReal(x):
    if x is None:
        return None
    return x * 2.0

def addOptInt(x, y):
    if x is None or y is None:
        return None
    return x + y
