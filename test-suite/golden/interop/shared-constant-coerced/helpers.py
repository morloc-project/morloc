import os

_LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), "tick.log")


def tick(x):
    with open(_LOG, "a") as fh:
        fh.write("tick\n")
    return x


def ticks(_):
    if not os.path.exists(_LOG):
        return 0
    with open(_LOG) as fh:
        return len(fh.readlines())
