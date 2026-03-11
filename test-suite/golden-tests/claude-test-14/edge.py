_counter = 0

def counter():
    global _counter
    _counter += 1
    return _counter

def addTwo(a, b):
    return a + b

def doubleIt(x):
    return x * 2

def makePair(a, b):
    return [a, b]

def replicateStr(n, s):
    return s * n

def lenStr(s):
    return len(s)

def sumVec(xs):
    return sum(xs)

def appendVec(a, b):
    return list(a) + list(b)

def incVec(xs):
    return [x + 1 for x in xs]
