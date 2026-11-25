import os


def tick(path):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)


def count(path):
    return os.path.getsize(path) if os.path.exists(path) else 0


def h_pass(f, x):
    return f(x)
