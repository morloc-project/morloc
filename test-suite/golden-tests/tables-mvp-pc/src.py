import pyarrow as pa


def makeT(n):
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [str(i) for i in range(n)],
    })
