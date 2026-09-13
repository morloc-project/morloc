import pyarrow as pa


def mkTable(n):
    return pa.table({"x": [1, 2, 3], "y": ["a", "b", "c"]})


def kindOf(t):
    return type(t).__module__ + "." + type(t).__name__


def addRow(t):
    # Consumes a pyarrow.Table and returns one; a RecordBatch would not
    # have .to_batches().
    extra = pa.table({"x": pa.array([4], t.schema.field("x").type), "y": ["d"]})
    return pa.concat_tables([t, extra])
