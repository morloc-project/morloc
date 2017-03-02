#!/usr/bin/env python3

import sys
import os
import subprocess

outdir = "/home/shoggoth/src/git/loc/tests/json/locout"

output_type = {
    'm0' : 'Int', 
    'm1' : 'String', 
    'm2' : '[Int]'
}

def a_int():
    return 5

def a_str():
    return "hi"

def a_arr():
    return [1,2]



def m0():
    return a_int()

def m1():
    return a_str()

def m2():
    return a_arr()



def show_m0():
    x = m0()
    s = '{"type":"Int", "value":"%s"}' % str(x)
    return s 


def show_m1():
    x = m1()
    s = '{"type":"String", "value":"%s"}' % str(x)
    return s 


def show_m2():
    x = m2()
    val = '[{}]'.format(','.join('"%s"' % str(y) for y in x))
    s = '{"type":"[Int]", "value":%s}' % val
    return s 




def read_m0():
    x = m0()
    None
    return s


def read_m1():
    x = m1()
    None
    return s


def read_m2():
    x = m2()
    None
    return s


if __name__ == '__main__':
    args = sys.argv
    cmd_str = "{function}({args})"
    arg_str = ', '.join(args[2:])
    cmd = cmd_str.format(function="show_" + args[1], args=arg_str)
    try:
        print(eval(cmd))
    except SyntaxError as e:
        print("Syntax error in:\n%s\n%s" % (cmd, e), file=sys.stderr)

