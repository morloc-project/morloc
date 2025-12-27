def fooFun(algconf, xs):
    return sum(xs) + algconf["m"] + algconf["n"] * 100

def barFun(sysconf, algconf, yolo, xs):
    return sum(xs) + len(yolo) + 100 * sysconf["numThreads"] + 1000 * algconf["m"]

def travelTime(v,n,w,b,d):
    return w * b * d + 5 * v + n

def bif(algconf):
    return str(algconf["removeCaches"])
