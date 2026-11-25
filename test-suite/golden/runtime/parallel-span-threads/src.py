import pyarrow as pa


def makeTable(i):
    n = 50000
    return pa.RecordBatch.from_pydict({
        "idx": list(range(n)),
        "value": [float(i) + 0.5 * k for k in range(n)],
    })


def bigList(i):
    return [float(i)] * 20000


def addHeadLen(xs, x):
    return x + xs[0] + len(xs)
