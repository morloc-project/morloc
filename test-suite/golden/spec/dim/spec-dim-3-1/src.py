def size(xs):
    return len(xs)

def first(p):
    return len(p[0])

def fieldlen(r):
    return len(r["xs"])

def optlen(x):
    return -1 if x is None else len(x)

def big_rows(n):
    return [[1, 2] for _ in range(n - 1)] + [[1, 2, 3]]
