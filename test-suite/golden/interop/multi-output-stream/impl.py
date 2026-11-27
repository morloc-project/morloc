def produce(log, sink):
    with open(log, "a") as f:
        f.write("ran\n")
    sink([1, 2])
    sink([])
    sink([3, 4, 5])
    sink(list(range(20000)))

def produce_tail(sink):
    sink([7])

def join_all(xs):
    return "%d: %s\n" % (len(xs), ",".join(str(x) for x in xs[:8]))

def show_batch(xs):
    return "batch of %d\n" % len(xs)

def zero():
    return (0, [])

def add_batch(acc, xs):
    return (acc[0] + sum(xs), acc[1] + [len(xs)])

def merge(a, b):
    return (a[0] + b[0], a[1] + b[1])

def show_acc(acc):
    return "sum=%d sizes=%s\n" % (acc[0], ",".join(str(s) for s in acc[1]))
