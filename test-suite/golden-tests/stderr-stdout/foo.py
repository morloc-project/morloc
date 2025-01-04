import sys

def pfoo(name):
    print("STDERR Hello " + name, file=sys.stderr)
    print("STDOUT Hello " + name)
