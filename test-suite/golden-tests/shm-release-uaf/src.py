def readingsPy(i):
    return list(range(i, i + 3000))

def totalPy(xs):
    return sum(xs)

def pairTotalPy(a, b):
    return sum(a) * 7 + sum(b)

def addTotalPy(xs, x):
    return sum(xs) + x

def applyNPy(f, n):
    return sum(f(i) for i in range(n))

def idPy(xs):
    return xs
