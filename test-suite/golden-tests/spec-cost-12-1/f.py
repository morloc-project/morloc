import sys

def py_step(k):
    print("py", k, file=sys.stderr, flush=True)
    return k + 1

def py_pure(k):
    print("pure", k, file=sys.stderr, flush=True)
    return k * 10

def py_fail(k):
    print("fail", k, file=sys.stderr, flush=True)
    raise ValueError("boom")
