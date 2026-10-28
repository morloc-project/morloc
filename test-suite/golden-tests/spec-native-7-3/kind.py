def kind(xs):
    return type(xs).__name__ + " " + str(xs.dtype)

def kind_map(m):
    return type(m).__name__ + " " + " ".join(kind(v) for v in m.values())

def kind_matrix(m):
    return type(m).__name__ + " " + str(m.dtype) + " " + str(m.shape)
