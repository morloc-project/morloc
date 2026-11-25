def produce(sink):
    sink([1, 2])
    sink([])
    sink([3, 4, 5])


def inc(xs):
    return [x + 10 for x in xs]
