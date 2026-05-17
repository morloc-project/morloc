def foo(x):
    if x < 0:
        raise ValueError("negative input")
    return x * 2

def handle_error(thunk):
    try:
        return thunk()
    except Exception:
        return -1
