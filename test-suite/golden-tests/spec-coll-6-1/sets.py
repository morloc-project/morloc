NAMES = ["pear", "apple", "fig", "kiwi", "date"]


def forward(n):
    s = set()
    for x in NAMES[:n]:
        s.add(x)
    return s


def backward(n):
    s = set()
    for x in reversed(NAMES[:n]):
        s.add(x)
    return s
