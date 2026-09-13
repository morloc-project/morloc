import time

def slow_io(x):
    time.sleep(1)
    return x

def run_twice(t):
    return t() + t()
