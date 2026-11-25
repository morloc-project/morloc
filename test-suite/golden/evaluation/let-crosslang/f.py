def makeStr(n):
    return "y" * n

def strLen(s):
    return len(s)

def doubleIt(x):
    return x * 2

def addTwo(a, b):
    return a + b

def makeRange(n):
    return list(range(1, n + 1))

def sumList(xs):
    return sum(xs)

def incAll(xs):
    return [x + 1 for x in xs]

def idStr(s):
    return s

def idList(xs):
    return list(xs)

_counter = 0
def counter():
    global _counter
    _counter += 1
    return _counter
