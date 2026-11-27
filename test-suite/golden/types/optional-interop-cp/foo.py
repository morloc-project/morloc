def pSafeDiv(x, y):
    if y == 0:
        return None
    return x // y

def pFromNull(default_val, x):
    if x is None:
        return default_val
    return x

def pDouble(x):
    if x is None:
        return None
    return x * 2
