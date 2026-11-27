import sys

def reals(_):
    return [0.1 + 0.2, 1 / 3, 2.0 ** 53 + 2, sys.float_info.max, 5e-324, 1e15, 1e16, 1e-5, 1e-4, 123.0, -0.0, 0.1]

def echo(xs):
    return xs
