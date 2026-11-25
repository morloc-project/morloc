import os
import threading


def produce_in_thread(sink):
    sink([1, 2])
    # The default excepthook prints the put's error and the thread ends.
    t = threading.Thread(target=lambda: sink(["not an int"]))
    t.start()
    t.join()
    sink([3])


def produce_in_child(sink):
    sink([1, 2])
    pid = os.fork()
    if pid == 0:
        try:
            sink(["not an int"])
        except BaseException:
            pass
        os._exit(0)
    os.waitpid(pid, 0)
    sink([3])
