def mk(i): return [i, i + 10]
def zeroAcc(): return set()
def addBatch(a, b): return a | set(b)
def mergeAcc(a, b): return a | b
def showAcc(a): return " ".join(str(x) for x in sorted(a))
