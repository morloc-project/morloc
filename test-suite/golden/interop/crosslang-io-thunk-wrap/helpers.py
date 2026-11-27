import sys


def py_io(x):
    print(f"py:{x}", file=sys.stderr)
    return x + 100


def py_gate(x):
    return x > 0


def py_io2(a, b):
    print(f"py2:{a},{b}", file=sys.stderr)
    return a + b + 100


def py_io_error(x):
    if x < 0:
        raise RuntimeError(f"py_io_error: negative x={x}")
    print(f"pyerr:{x}", file=sys.stderr)
    return x + 100
