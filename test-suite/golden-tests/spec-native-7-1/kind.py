def kind(xs):
    return type(xs).__name__ + " " + " ".join(type(x).__name__ for x in xs)

def kind_dirs(xs):
    return type(xs).__name__ + " " + " ".join(type(x).__name__ for x in xs)
