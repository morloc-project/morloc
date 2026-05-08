import pyarrow as pa


def readData(n):
    """Generate n rows with id = i and value = i * 0.5."""
    return pa.RecordBatch.from_pydict({
        "id": list(range(n)),
        "value": [float(i) * 0.5 for i in range(n)],
    })


def lookupRow(i, t):
    """Return the value at row index i."""
    return float(t.column("value")[i].as_py())
