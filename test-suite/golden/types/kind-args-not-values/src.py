import pyarrow as pa


def mk(n):
    return pa.RecordBatch.from_pydict(
        {"x": list(range(n)), "y": [str(i) for i in range(n)]}
    )


def describe(t):
    return f"{t.num_rows} rows x={t.column('x').to_pylist()} y={t.column('y').to_pylist()}"


def sumA(ps):
    return sum(p["a"] for p in ps)

