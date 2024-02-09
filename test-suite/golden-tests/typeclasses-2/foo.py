def addInt(x, y):
    return x + y

def addReal(x, y):
    return x + y

def addStr(x, y):
    return x + y

def fold(f, b, xs):
    for x in xs:
        b = f(b, x)
    return b
