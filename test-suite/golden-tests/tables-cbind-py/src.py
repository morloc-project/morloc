import pyarrow as pa


def makeX(n):
    return pa.RecordBatch.from_pydict({"x": list(range(n))})


def makeY(n):
    return pa.RecordBatch.from_pydict({"y": [str(i) for i in range(n)]})
