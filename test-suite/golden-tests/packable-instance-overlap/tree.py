def pack_generic(triple):
    nodes, edges, leafs = triple
    return {"nodes": nodes, "edges": edges, "leafs": leafs}


def unpack_generic(t):
    return [t["nodes"], t["edges"], t["leafs"]]


def pack_specific(triple):
    nodes, edges, leafs = triple
    return {"nodes": [n.upper() for n in nodes], "edges": edges, "leafs": leafs}


def unpack_specific(t):
    return [t["nodes"], t["edges"], t["leafs"]]


def root_node(t):
    return t["nodes"][0]


def pack_closure(triple):
    nodes, edges, leafs = triple
    return {"nodes": ["<" + n + ">" for n in nodes], "edges": edges, "leafs": leafs}


def unpack_closure(t):
    return [t["nodes"], t["edges"], t["leafs"]]


def pack_blob(xs):
    return {"nodes": xs, "edges": [], "leafs": []}


def unpack_blob(t):
    return t["nodes"]
