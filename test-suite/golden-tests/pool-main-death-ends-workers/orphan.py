import os
import signal
import subprocess
import time


def orphan(seconds):
    parent = os.getppid()
    args = subprocess.run(["ps", "-o", "args=", "-p", str(parent)], capture_output=True, text=True).stdout
    if "pool.py" not in args:
        raise RuntimeError("this pool runs calls in its main process")
    os.kill(parent, signal.SIGKILL)
    time.sleep(seconds)
    return seconds
