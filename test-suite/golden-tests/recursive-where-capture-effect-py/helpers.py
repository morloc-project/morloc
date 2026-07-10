import sys

def noisy_add(x):
    print(f"noisy:{x}", file=sys.stderr)
    return x + 100
