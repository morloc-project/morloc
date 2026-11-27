def boom(x):
    raise ValueError("boom")

def safe(f, x):
    try:
        return f(x)
    except ValueError:
        return -1
