def pack_generic(xs):
    return {"v": xs, "how": "generic"}


def unpack_generic(d):
    return d["v"]


def pack_special(xs):
    return {"v": xs, "how": "special"}


def unpack_special(d):
    return d["v"]


def pack_int(xs):
    return {"v": xs, "how": "int-instance"}


def unpack_int(d):
    return d["v"]


def pack_str(xs):
    return {"v": xs, "how": "str-instance"}


def unpack_str(d):
    return d["v"]


def how(d):
    return d["how"]


def make_tree(s):
    return {"nodes": [s, "b"], "edges": [[0, 1, "e"]], "leafs": ["x"]}


def pack_a(triple):
    nodes, edges, leafs = triple
    return {"nodes": nodes, "edges": edges, "leafs": leafs}


def unpack_a(t):
    return [t["nodes"], t["edges"], t["leafs"]]


def pack_b(triple):
    nodes, edges, leafs = triple
    return {"nodes": nodes, "edges": edges, "leafs": leafs}


def unpack_b(t):
    return [[n.upper() for n in t["nodes"]], t["edges"], t["leafs"]]
