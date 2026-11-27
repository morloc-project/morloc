import sys

def eq(x, y):
    if x == y:
        return True
    else:
        print("Test failed:", file=sys.stderr)
        print(f"  Expected: {y!s}", file=sys.stderr)
        print(f"  Observed: {x!s}", file=sys.stderr)
        return False

def check(xs):
    for (msg, x) in xs:
        if not x:
            return False
    return True
