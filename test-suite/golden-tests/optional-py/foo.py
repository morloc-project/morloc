def isNull(x):
    return x is None

def fromNull(default_val, x):
    if x is None:
        return default_val
    return x

def toNull(x):
    return x

def safeHead(xs):
    if len(xs) == 0:
        return None
    return xs[0]

def optionalAdd(x, y):
    if x is None or y is None:
        return None
    return x + y

def optionalList(xs):
    return [None if x < 0 else x for x in xs]

def countNulls(xs):
    return sum(1 for x in xs if x is None)
