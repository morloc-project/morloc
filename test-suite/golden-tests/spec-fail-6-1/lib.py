import os

def die(x):
    os._exit(9)

def guard(f, x):
    try:
        return f(x)
    except BaseException:
        return -1
