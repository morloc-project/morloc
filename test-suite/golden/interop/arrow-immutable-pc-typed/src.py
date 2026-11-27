import pyarrow as pa

def makeLargeTable(n):
    return pa.RecordBatch.from_pydict({
        "idx": list(range(n)),
        "value": [float(i) * 0.5 for i in range(n)]
    })

def makeIndices(n):
    return list(range(n))

def sumReals(xs):
    return sum(xs)
