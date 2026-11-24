import multiprocessing
import os


def seq_p(sink):
    sink([1, 2])
    pid = os.fork()
    if pid == 0:
        sink([5])
        os._exit(0)
    os.waitpid(pid, 0)
    sink([3])


def returns_p(sink):
    sink([1])
    pid = os.fork()
    if pid == 0:
        sink([5])
        return
    os.waitpid(pid, 0)
    sink([3])


def pool_p(sink):
    ctx = multiprocessing.get_context("fork")
    ps = [ctx.Process(target=sink, args=([i],)) for i in range(1, 5)]
    for p in ps:
        p.start()
    for p in ps:
        p.join()


def raises_p(sink):
    sink([1])
    pid = os.fork()
    if pid == 0:
        sink([5])
        raise ValueError("the child raises")
    os.waitpid(pid, 0)
    sink([3])
