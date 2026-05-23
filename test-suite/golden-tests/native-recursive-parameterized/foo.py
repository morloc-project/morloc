# Trie with Int payload, list-of-(key, child) children. Build a
# balanced depth-d trie: every internal node has two children keyed by
# "L" and "R"; leaves carry val=0 (None is also acceptable but we keep
# a payload here so the wire form has bytes to move).
def build_trie(d):
    if d <= 0:
        return {"val": 0, "subs": []}
    sub = build_trie(d - 1)
    return {"val": None, "subs": [("L", sub), ("R", sub)]}


def trie_size(t):
    n = 1
    for _, child in t["subs"]:
        n += trie_size(child)
    return n


# Two-parameter analog: a TreeKV Str Int. Same shape as build_trie.
def build_treekv(d):
    if d <= 0:
        return {"key": "leaf", "value": 1, "children": []}
    sub = build_treekv(d - 1)
    return {"key": "inner", "value": 0,
            "children": [("L", sub), ("R", sub)]}


def tree_size(t):
    n = 1
    for _, child in t["children"]:
        n += tree_size(child)
    return n


# Build a 2-level trie of branching n: root has n children, each leaf.
# Total nodes = n + 1.
def build_two_level(n):
    leaves = [(f"k{i}", {"val": i, "subs": []}) for i in range(n)]
    return {"val": None, "subs": leaves}
