def pmakeStr(n):
    return "y" * n

def pstrLen(s):
    return len(s)

def psumList(xs):
    return sum(xs)

def pmakeRange(n):
    return list(range(1, n + 1))

def pincAll(xs):
    return [x + 1 for x in xs]

def pidStr(s):
    return s

def pidList(xs):
    return list(xs)

def pdoubleIt(x):
    return x * 2

def paddTwo(a, b):
    return a + b

def pmakePair(a, b):
    return [a, b]

def preplicateStr(n, s):
    return s * n

def pappendVec(a, b):
    return list(a) + list(b)

def pvecOfStrs(n, m):
    return ["B" * m] * n

def psumStrLens(vs):
    return sum(len(s) for s in vs)

def pnestedVec(outer, inner):
    row = list(range(1, inner + 1))
    return [list(row) for _ in range(outer)]

def pnestedSum(vv):
    return sum(x for v in vv for x in v)

_counter = 0
def pcounter():
    global _counter
    _counter += 1
    return _counter
