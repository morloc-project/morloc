def apply2(k, a, b):
    return k(a, b)

def feed(k):
    return k(lambda a, b: a - b)

def make_sub(n):
    return lambda a, b: (a - b) * n

def first2(fs, x):
    return fs[0](x, x)

def feed_split(k):
    return k(lambda a: (lambda y: a * 10 + y))
