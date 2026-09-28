import pyarrow as pa

def mk_pair(n):
    return (n, pa.RecordBatch.from_pydict({"x": list(range(n))}))
