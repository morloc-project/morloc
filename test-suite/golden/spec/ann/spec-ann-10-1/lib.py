import sys

def load(x):
    sys.stderr.write("RUN load %d\n" % x)
    return x

def run_twice(f):
    return f() + f()
