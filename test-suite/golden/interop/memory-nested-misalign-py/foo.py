def toNull(x):
    return x

def makeOptRecord(flag, value):
    return {"flag": flag, "value": value}

def fromNullRecord(default_val, x):
    if x is None:
        return default_val
    return x

def sumRecordValues(records):
    return sum(r["value"] for r in records)

def makeRecordWithOpt(flag, opt):
    return {"flag": flag, "opt": opt}

def getRecordOpt(r):
    return r["opt"]

def nestedRoundTrip(x):
    if x is None:
        return None
    return {"flag": x["flag"], "value": x["value"]}
