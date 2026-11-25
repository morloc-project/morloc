import sys

def py_echo(x):
    print("P" + str(x), file=sys.stderr, flush=True)
    return x + 1

def py_add(a, b):
    return a + b
