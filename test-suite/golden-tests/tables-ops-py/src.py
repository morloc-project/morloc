import pyarrow as pa


def mkData(n):
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [f"r{i}" for i in range(n)],
    })
