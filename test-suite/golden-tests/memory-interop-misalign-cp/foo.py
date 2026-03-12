def pMakeOptReal(x):
    return x

def pFromNullReal(default_val, x):
    if x is None:
        return default_val
    return x

def pMakePair(flag, value):
    return {"flag": flag, "value": value}

def pGetPairValue(p):
    return p["value"]

def pDoublePairValue(p):
    return {"flag": p["flag"], "value": p["value"] * 2.0}
