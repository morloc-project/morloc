import sys

def nextInt(k):
    print("nextInt", k, file=sys.stderr)
    return k + 6

def savePoint(xs):
    print("savePoint", xs, file=sys.stderr)
