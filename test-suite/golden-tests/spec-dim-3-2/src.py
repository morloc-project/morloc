def mk(n):
    return list(range(n))

def dot(a, b):
    return sum(x * y for x, y in zip(a, b))

def roll(w, v):
    return [sum(v[i:i + w]) for i in range(len(v) - w + 1)]

def rep(n, x):
    return [x] * n

def pair_len(p):
    return len(p[1])

def first_opt(p):
    return 0 if p is None else len(p[0])

def opts_len(o):
    return len(o["left"])

def deep(xs, v):
    return len(v)

def grow(a, b):
    return len(b)

def named(s, a, b):
    return sum(x * y for x, y in zip(a, b))
