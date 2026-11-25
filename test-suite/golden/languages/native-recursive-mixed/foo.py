# Helper: build a Pair Int of length k iteratively.
def _build_pair(k):
    node = (0, None)
    for _ in range(k):
        node = (1, node)
    return node


# Build a chain of Nodes of depth n. Each Node carries a Pair Int of
# length 3 in `chain`. The leftmost Node terminates with left=None;
# all earlier Nodes carry a `left` link to the next.
def build_node(n):
    cur = {"ident": 0, "chain": _build_pair(3), "left": None}
    for i in range(n):
        cur = {"ident": i + 1, "chain": _build_pair(3), "left": cur}
    return cur


# Build a (Int, LL) tuple where the LL has length n.
def build_wrapped_ll(n):
    ll = {"head": 0, "tail": None}
    for i in range(n):
        ll = {"head": i + 1, "tail": ll}
    return (42, ll)
