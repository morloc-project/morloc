import time

def wait(path):
    with open(path, "w") as f:
        f.write("started")
    time.sleep(60)
    return 1


def pong(x):
    return x
