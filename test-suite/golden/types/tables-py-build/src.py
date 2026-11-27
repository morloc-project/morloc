import pyarrow as pa


def _make_xy(n):
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [str(i) for i in range(n)],
    })


# Two morloc-level bindings for the same Python builder so the merged
# .loc can attach a kindless and a fully-typed signature to it.
makeXY = _make_xy
makeXY_typed = _make_xy


def makeXYZ(n):
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [str(i) for i in range(n)],
        "z": [float(i) * 0.5 for i in range(n)],
    })
