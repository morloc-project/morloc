def bigStr(n):
    return "A" * n

def bigStrLen(s):
    return len(s)

def bigVec(n):
    return list(range(n))

def bigVecSum(v):
    return sum(v) % 1000000007

def vecOfStrs(n, m):
    return ["B" * m] * n

def sumStrLens(vs):
    return sum(len(s) for s in vs)

def nestedVec(outer, inner):
    row = list(range(1, inner + 1))
    return [list(row) for _ in range(outer)]

def nestedSum(vv):
    return sum(x for v in vv for x in v)

def idVecStr(vs):
    return list(vs)

def idNestedVec(vv):
    return [list(v) for v in vv]
