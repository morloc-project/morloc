_LOG = "/tmp/eval_sugar_log.txt"
_COUNTER = [0]


def reset_log():
    _COUNTER[0] = 0
    try:
        open(_LOG, "w").close()
    except OSError:
        pass


def bump_a():
    _COUNTER[0] += 1
    with open(_LOG, "a") as f:
        f.write("A" + str(_COUNTER[0]) + "\n")
    return _COUNTER[0]


def bump_b():
    _COUNTER[0] += 1
    with open(_LOG, "a") as f:
        f.write("B" + str(_COUNTER[0]) + "\n")
    return _COUNTER[0]


def read_log():
    try:
        with open(_LOG) as f:
            return ",".join(line.strip() for line in f)
    except OSError:
        return ""


def add_i(a, b):
    return a + b


def double_it(n):
    return n * 2
