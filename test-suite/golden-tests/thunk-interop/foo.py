import sys

def sideEffectPy(x):
    print("EVAL_PY " + str(x))
    sys.stdout.flush()
    return x * 2

def addPy(a, b):
    return a + b
