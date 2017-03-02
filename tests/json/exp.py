#!/usr/bin/env python3

import sys
import os
import subprocess

output_type = {
    'm0' : 'Int', 
    'm1' : 'Int'
}

def foo():
    return 5

def bar(x):
    return x * 2


def m0():
    return bar(m1())

def m1():
    return foo()

def show_m0():
    return '{"type"="Int", "value"=%s}' % m0()

if __name__ == '__main__':
    args = sys.argv
    cmd_str = "{function}({args})"
    arg_str = ', '.join(args[2:])
    cmd = cmd_str.format(function="show_" + args[1], args=arg_str)
    try:
        print(eval(cmd))
    except SyntaxError as e:
        print("Syntax error in:\n%s\n%s" % (cmd, e), file=sys.stderr)
