def bare(s):
    return s + chr(0) + s


def in_list(s):
    return [s, s + chr(0) + s]


def in_record(s):
    return {"a": 1, "b": s + chr(0) + s}


def clean(s):
    return s + s


def no_str(s):
    return len(s)
