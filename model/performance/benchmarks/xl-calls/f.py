def inc(x):
    return x + 1


def mapf(f, n):
    s = 0
    for i in range(n):
        s += f(i)
    return s


def native(n):
    s = 0
    for i in range(n):
        s += inc(i)
    return s
