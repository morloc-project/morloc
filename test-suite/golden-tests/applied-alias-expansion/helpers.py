def sourcedBox(h):
    return {"x": h, "y": 9}

def boxSize(b):
    return b["y"] * 10

def sourcedY(b):
    return b["y"]

def sourcedQ(r):
    return r["q"]

def bump(i):
    return i + 1

def bumpTwice(i):
    return i + 2

def bumpLib(i):
    return i + 3

def applyGrouped(f, x):
    return f(x)(x)
