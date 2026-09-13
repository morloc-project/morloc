import sys

def get(n):
    return n

def put(n):
    print("put", n, file=sys.stderr)
    return n

def run_twice(t):
    return t() + t()
