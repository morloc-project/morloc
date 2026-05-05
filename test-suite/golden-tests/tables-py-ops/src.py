import pyarrow as pa


def mkData(n):
    # ops fixture: x = 0..n-1, y = "r0".."r(n-1)"
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [f"r{i}" for i in range(n)],
    })


def makeXY(n):
    # rbind / add-drop fixture: y = str(i)
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [str(i) for i in range(n)],
    })


def makeX(n):
    return pa.RecordBatch.from_pydict({"x": list(range(n))})


def makeY(n):
    return pa.RecordBatch.from_pydict({"y": [str(i) for i in range(n)]})


def makeXYZ(n):
    return pa.RecordBatch.from_pydict({
        "x": list(range(n)),
        "y": [str(i) for i in range(n)],
        "z": [float(i) * 0.5 for i in range(n)],
    })


def makeZ(n):
    return [float(i) * 0.5 for i in range(n)]
