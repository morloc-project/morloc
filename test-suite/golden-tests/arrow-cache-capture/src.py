import pyarrow as pa


def mk(n):
    with open("mk.calls", "a") as f:
        f.write("call\n")
    return pa.RecordBatch.from_arrays(
        [pa.array([1, 2, 3][:n], pa.int64()), pa.array(["a", "b", "c"][:n])],
        names=["x", "y"])


def describe(t):
    with open("describe.calls", "a") as f:
        f.write("call\n")
    return "; ".join("%s:%s" % (n, c.to_pylist()) for n, c in zip(t.column_names, t.columns))


def rowAt(t, k):
    return "x=%d y=%s" % (t.column("x")[k].as_py(), t.column("y")[k].as_py())


def tick():
    return 0


def mkListIO(n):
    return list(range(n))


def vsum(xs):
    return sum(xs)


def addInt(a, b):
    return a + b


def applyItPy(f, k):
    return f(k)
