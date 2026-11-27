import sys

def add1(x):
    return x + 1

def noisy_add(x):
    print(f"noisy:{x}", file=sys.stderr)
    return x + 100
