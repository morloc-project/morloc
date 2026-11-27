# Build a Pair Int chain of length n with all payloads = 1 except the
# terminator (0, None). Recursive for parity with the original test.
def build_pair(n):
    if n <= 0:
        return (0, None)
    return (1, build_pair(n - 1))


# Iterative builder for stress tests. Python's recursion limit caps
# recursive builds at ~1000 frames; this version walks a loop so the
# build side never trips that ceiling.
def build_pair_deep(n):
    node = (0, None)
    for _ in range(n):
        node = (1, node)
    return node


# Balanced BTree of depth d: every internal node has two children, every
# leaf carries value 1, and internal nodes carry 0. Total node count
# for depth d is 2**(d+1) - 1.
def build_btree(d):
    if d <= 0:
        return (1, None, None)
    sub = build_btree(d - 1)
    return (0, sub, sub)


# Two-parameter Pair2 with payload (Int, Str) at every level. Same
# right-spined shape as build_pair; the Str field is constant.
def build_pair2(n):
    node = (0, "end", None)
    for _ in range(n):
        node = (1, "x", node)
    return node
