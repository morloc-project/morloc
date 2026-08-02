def mul(a, b):
    return a * b


def applyIt(f, x):
    return f(x)


def makeAndApply(g, x):
    return applyIt(g(x), x)
