import pyarrow as pa

ROWS = 524288  # one 4 MiB Int64 column


def mkChunk(i):
    base = i * ROWS
    return pa.RecordBatch.from_arrays(
        [pa.array(range(base, base + ROWS), pa.int64())], names=["x"])


def nRows(t):
    return t.num_rows
