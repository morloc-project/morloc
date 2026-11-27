class Lbl:
    def __init__(self, v):
        self.v = v


def pack_lbl(x):
    return Lbl(x)


def unpack_lbl(x):
    return x.v


def mk_tree(k):
    if k <= 0:
        return {"label": Lbl(1), "children": []}
    return {"label": Lbl(k), "children": [mk_tree(k - 1), mk_tree(k - 1)]}


def sum_tree(t):
    return t["label"].v + sum(sum_tree(c) for c in t["children"])
