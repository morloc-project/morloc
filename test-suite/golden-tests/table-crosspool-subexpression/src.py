import pyarrow as pa

def mk(n):
    return pa.record_batch({"x": pa.array(list(range(n)))})
