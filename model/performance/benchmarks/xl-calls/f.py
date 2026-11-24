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


def touch(r):
    return r["ra"] + r["rc"]


def maprec(f, n):
    s = 0
    for i in range(n):
        s += f({"ra": i, "rb": 1.0, "rc": 2, "rd": 3.0, "re": 4, "rf": 5.0, "rg": 6, "rh": 7.0})
    return s


def vsum(xs):
    return sum(xs)


def mapvec(f, m, n):
    xs = [float(i) for i in range(m)]
    s = 0.0
    for _ in range(n):
        s += f(xs)
    return s
