import sys

def emit(a, b, sink):
    sink([a, b])

def say(xs):
    print("py", xs, file=sys.stderr)
