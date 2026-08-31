def py_maybe_list(s):
    if not s:
        raise ValueError("py_maybe_list: empty input")
    return [len(s)]
