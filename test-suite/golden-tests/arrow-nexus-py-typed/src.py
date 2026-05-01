import pyarrow as pa

def makePeople():
    return pa.RecordBatch.from_pydict({"name": ["Alice", "Bob"], "age": [30, 25]})
