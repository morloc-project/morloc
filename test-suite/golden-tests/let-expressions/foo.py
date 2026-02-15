import sys

def sideEffectPy(x):
    print("EVAL " + str(x), file=sys.stderr)
    return x * 2
