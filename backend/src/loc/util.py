import sys
import re

def err(msg):
    sys.exit(msg)

def indent(text, n=4):
    if text:
        lines = text.split('\n')
        s = "\n".join([' ' * n + l for l in lines])
    else:
        s = text
    return s

def clean(text):
    s = []
    for line in text.split('\n'): 
        line = line.rstrip()
        if(len(line) != 0):
            s.append(line)
    return '\n'.join(s)