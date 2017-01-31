import sys

def err(msg):
    sys.exit(msg)

def indent(text, n=4):
    if text:
        lines = text.split('\n')
        s = "\n".join([' ' * n + l for l in lines])
    else:
        s = text
    return s
