def double(x):
    return 2 * x


def incr(x):
    return x + 1


def add2(a, b):
    return a + b


def apply_py(f, x):
    return f(x)


def slow(x):
    import sys
    sys.stderr.write("SLOW " + str(x) + "\n")
    return x * 10
