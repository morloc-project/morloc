import sys

def runTest(xs):
    if(all(xs)):
        print("success", file=sys.stderr)
        return True
    else:
        print("fail", file=sys.stderr)
        return False
