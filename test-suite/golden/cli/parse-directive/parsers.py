def readCsvPy(path):
    with open(path) as fh:
        return [int(x) for x in fh.read().strip().split(",")]


def producePy(path, sink):
    batch = []
    with open(path) as fh:
        for line in fh:
            batch.append(int(line))
            if len(batch) == 3:
                sink(batch)
                batch = []
    if batch:
        sink(batch)


def sumPy(xs):
    return sum(xs)


def sizePy(xs):
    return len(xs)


def sumOpt(xs, n):
    return n if xs is None else n + sum(xs)


def showTotal(n):
    return "total is %d\n" % n


def mark(path):
    with open(path, "w") as fh:
        fh.write("ran\n")


def countStrs(xs):
    return len(xs)


def strLen(s):
    return len(s)
