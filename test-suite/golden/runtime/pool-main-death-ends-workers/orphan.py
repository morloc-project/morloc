import os
import signal
import subprocess
import time


def orphan(seconds):
    parent = os.getppid()
    args = subprocess.run(["ps", "-o", "args=", "-p", str(parent)], capture_output=True, text=True).stdout
    if "pool.py" not in args:
        raise RuntimeError("the call is not running in a worker of the pool")
    os.kill(parent, signal.SIGKILL)
    time.sleep(seconds)
    return seconds
