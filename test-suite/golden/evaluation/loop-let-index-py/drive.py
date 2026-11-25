def drive(produce):
    out = []
    def sink(xs):
        out.extend(xs)
    produce(sink)
    return out

def tag(xs):
    return [x * 10 for x in xs]
