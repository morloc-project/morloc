import os


def apply2(f, x):
    return f(x) + f(x)


def tick(x):
    with open(os.environ["TICK_LOG"], "a") as fh:
        fh.write("tick\n")
    return x


def ticks(_):
    if not os.path.exists(os.environ["TICK_LOG"]):
        return 0
    with open(os.environ["TICK_LOG"]) as fh:
        return len(fh.readlines())
