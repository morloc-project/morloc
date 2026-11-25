def funf(a):
    def f(x):
        return a*x
    return f

def fung(b):
    def g(x):
        return b*x
    return g

def bar(a, b):
    return { "f": funf(a), "g": fung(b) }
