import sys

def nextInt(k):
    print("nextInt", k, file=sys.stderr)
    return k + 6

def double(k):
    print("double", k, file=sys.stderr)
    return k * 2

def takeThunk(t):
    kind = "callable" if callable(t) else "value"
    print("takeThunk received a", kind, file=sys.stderr)
    return t() + t()
