def makePair(flag, value):
    return {"flag": flag, "value": value}

def getPairFlag(p):
    return p["flag"]

def getPairValue(p):
    return p["value"]

def pairRoundTrip(p):
    return {"flag": p["flag"], "value": p["value"]}

def makeTriple(flag, count, value):
    return {"flag": flag, "count": count, "value": value}

def getTripleCount(t):
    return t["count"]

def getTripleValue(t):
    return t["value"]

def tripleRoundTrip(t):
    return {"flag": t["flag"], "count": t["count"], "value": t["value"]}
