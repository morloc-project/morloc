def sum_tree(t):
    total = t["value"]
    for c in t["children"]:
        total += sum_tree(c)
    return total


def ll_len(node):
    n = 0
    while node is not None:
        n += 1
        node = node["tail"]
    return n


# Build a balanced tree where each internal node has two children and
# each leaf carries value 1. value of an internal node is 0, so the
# whole tree's sum equals the number of leaves = 2 ** depth.
def build_tree(depth):
    if depth <= 0:
        return {"value": 1, "children": []}
    return {
        "value": 0,
        "children": [build_tree(depth - 1), build_tree(depth - 1)],
    }


# Build a linked list of length n with head values 1, 2, ..., n.
# Length 0 is represented by tail=None at the only node (head=0), so
# tests start at n>=1 for sensible counts.
def build_ll(n):
    if n <= 0:
        return {"head": 0, "tail": None}
    return {"head": 1, "tail": build_ll(n - 1)}


# Build a Container Int of length n with values 1, 2, ..., n. Same
# shape as build_ll but exercised through the parameterized Container a
# record. Iterative construction so we do not hit Python's recursion
# limit for the deeper traversal tests.
def build_container(n):
    node = {"val": 0, "sub": None}
    for _ in range(n):
        node = {"val": 1, "sub": node}
    return node
