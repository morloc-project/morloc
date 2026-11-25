import os
import signal


def pyOk(x):
    return x


def pyDie(x):
    os.kill(os.getpid(), signal.SIGKILL)
    return x


def pyApply(f, x):
    return f(x)
