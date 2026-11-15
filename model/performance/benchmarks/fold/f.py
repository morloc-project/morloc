def produce(nkeys, s, b, sink):
    x = 12345
    for _ in range(b):
        v = []
        for _ in range(s):
            x = (x * 6364136223846793005 + 1442695040888963407) & 0xFFFFFFFFFFFFFFFF
            v.append((x >> 33) % nkeys)
        sink(v)


def zero_n():
    return 0


def add_len(acc, v):
    return acc + len(v)


def plus(a, b):
    return a + b


def show_n(n):
    return f"{n}\n"


def zero_map():
    return {}


def add_map(m, v):
    for k in v:
        m[k] = m.get(k, 0) + 1
    return m


def plus_map(a, b):
    r = dict(a)
    for k, c in b.items():
        r[k] = r.get(k, 0) + c
    return r


def show_map(m):
    return f"{len(m)} {sum(m.values())}\n"


def control_map(nkeys, s, b):
    m = {}

    def sink(v):
        add_map(m, v)

    produce(nkeys, s, b, sink)
    return show_map(m)
