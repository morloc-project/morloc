import manifold

def get_sinks(manifolds):
    sinks = {k:True for k in manifolds.keys()}
    for k,m in manifolds.items():
        for k,n,v,t in m.input:
            try:
                sinks[v] = False
            except KeyError:
                err("This should not happen")
    sinks = [k for k,b in sinks.items() if b]
    return sinks

def _infer_type(manifolds, m):
    pass
    

def infer_types(manifolds):
    sinks = get_sinks(manifolds)

    for s in sinks:
        _infer_type(manifolds, manifolds[s])
