def packDate(s):
    return int(s)

def unpackDate(d):
    return str(d)

def showPairs(xs):
    return "py " + ",".join(str(p["fst"]) for p in xs) + "\n"
