import os
import sys


def _parent(pid):
    _, status = os.waitpid(pid, 0)
    code = os.WEXITSTATUS(status) if os.WIFEXITED(status) else 100 + os.WTERMSIG(status)
    return "parent:%d" % code


def pyForkReturns(x):
    pid = os.fork()
    if pid == 0:
        return "child"
    return _parent(pid)


def pyForkRaises(x):
    pid = os.fork()
    if pid == 0:
        raise RuntimeError("raised in the child")
    return _parent(pid)


def pyForkExits(x):
    pid = os.fork()
    if pid == 0:
        sys.exit(0)
    return _parent(pid)
