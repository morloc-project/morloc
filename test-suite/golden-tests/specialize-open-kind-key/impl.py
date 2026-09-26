import pyarrow as pa


def mkXY(n):
    return pa.table({"x": pa.array([1, 2, 3]), "y": pa.array(["a", "b", "c"])})


def step(t):
    return t


mkOpen = mkXY
mkPinned = mkXY
