import os
import signal


def crash(x):
    os.kill(os.getpid(), signal.SIGSEGV)
    return x
