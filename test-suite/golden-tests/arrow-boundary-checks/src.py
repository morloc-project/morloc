import pyarrow as pa
import pyarrow.parquet as pq


def floatsAsInt(n):
    # Declared {x = Int}; 1.5 cannot convert losslessly.
    return pa.RecordBatch.from_arrays([pa.array([1.5, 2.5], pa.float64())], names=["x"])


def wholeFloats(n):
    # Declared {x = Int}; 1.0 and 2.0 convert exactly.
    return pa.RecordBatch.from_arrays([pa.array([1.0, 2.0], pa.float64())], names=["x"])


def withNulls(n):
    return pa.RecordBatch.from_arrays([pa.array([1, None, 3], pa.int64())], names=["x"])


def int32s(n):
    # Declared {x = Int}; widened to int64 on the way out.
    return pa.RecordBatch.from_arrays([pa.array([1, 2, 3], pa.int32())], names=["x"])


def dictText(n):
    return pa.RecordBatch.from_arrays(
        [pa.array(["a", "b", "a"]).dictionary_encode()], names=["d"])


def dictInts(n):
    return pa.RecordBatch.from_arrays(
        [pa.array([10, 20, 10]).dictionary_encode()], names=["d"])


def reordered(n):
    # Declared {x = Int, y = Str}; produced as y, x, z.
    return pa.RecordBatch.from_arrays(
        [pa.array(["a", "b"]), pa.array([1, 2]), pa.array([0.5, 1.5])],
        names=["y", "x", "z"])


def describe(t):
    cols = []
    for name, col in zip(t.column_names, t.columns):
        cols.append("%s:%s:%s" % (name, col.type, col.to_pylist()))
    return "; ".join(cols)


def writeSnappy(path):
    pq.write_table(pa.table({"x": [1, 2, 3], "y": ["a", "b", "c"]}), path)
    return path


def numericIds(n):
    # Declared {id = Str}; an identifier stored as a number.
    return pa.RecordBatch.from_arrays([pa.array([7, 8], pa.int64())], names=["id"])
