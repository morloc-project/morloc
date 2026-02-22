import sys

def pySideEffect(x):
    print("EVAL " + str(x), flush=True)
    return x * 2
