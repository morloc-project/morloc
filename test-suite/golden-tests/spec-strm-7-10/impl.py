def produce_py(n, sink):
    for i in range(n):
        sink([100 + i])


def total(xs):
    return sum(xs)
