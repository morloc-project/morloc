import numpy as np
import pyarrow as pa


def mkTable(n):
    return pa.RecordBatch.from_arrays(
        [pa.array(np.arange(n, dtype=np.int64)), pa.array(np.arange(n, dtype=np.float64))],
        names=["x", "y"])


def nrows(t):
    return t.num_rows


def idPy(t):
    return t
