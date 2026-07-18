def loadA(n):
    if n == 1:
        return n * 10
    raise ValueError("loadA rejected")

def loadB(n):
    if n == 2:
        return n * 100
    raise ValueError("loadB rejected")

def loadC(n):
    if n == 3:
        return n * 1000
    raise ValueError("loadC rejected")
