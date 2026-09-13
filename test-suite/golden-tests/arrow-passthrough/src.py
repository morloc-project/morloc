import pyarrow as pa


def mk(n):
    return pa.RecordBatch.from_arrays(
        [pa.array([1, 2, 3, 4, 5], pa.int64()), pa.array(["a", "b", "c", "d", "e"])],
        names=["x", "y"])


def describe(t):
    return "; ".join("%s:%s" % (n, c.to_pylist()) for n, c in zip(t.column_names, t.columns))


def idPy(t):
    return t
