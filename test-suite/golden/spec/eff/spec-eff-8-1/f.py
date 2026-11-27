import sys

def say(x):
    print("py", x, file=sys.stderr)

def tick(x):
    print("py tick", x, file=sys.stderr)
    return x + 1

def log(n):
    print("py log", n, file=sys.stderr)
