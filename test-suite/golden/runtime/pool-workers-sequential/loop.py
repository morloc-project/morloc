def pyInc(x):
    return x + 1


def pyLoop(f, n):
    s = 0
    for i in range(n):
        s += f(i)
    return s
