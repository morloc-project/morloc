import sys

# Side-effecting functions: write a tag to stderr, return a deterministic value.
def py_echo(x):
    print("P" + str(x), file=sys.stderr, flush=True)
    return x + 1

def py_add(a, b):
    return a + b

def py_mul(a, b):
    return a * b

def py_pure(x):
    return x + 10

def py_id(x):
    return x

_py_counter = 0
def py_next_n():
    global _py_counter
    _py_counter += 1
    return _py_counter
