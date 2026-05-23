# Build a balanced rose tree of depth d. Each internal node has two
# children. Leaves carry value 1, internals carry value 0. Total node
# count for depth d is 2**(d+1) - 1 (perfect binary tree).
def build_rose(d):
    if d <= 0:
        return (1, [])
    sub = build_rose(d - 1)
    return (0, [sub, sub])


def rose_size(r):
    n = 1
    for c in r[1]:
        n += rose_size(c)
    return n


# Build a pure-structure Pat of depth d. The shape: at depth 0 it is
# the empty list `[]`; at depth d > 0 it is a list of two depth-(d-1)
# Pats. Carries no payload.
def build_pat(d):
    if d <= 0:
        return []
    sub = build_pat(d - 1)
    return [sub, sub]


def pat_nodes(p):
    n = 1
    for c in p:
        n += pat_nodes(c)
    return n
