def produce_rows(b, sink):
    for i in range(b):
        sink([[i * 10 + j, j] for j in range(i)])


def produce_strs(b, sink):
    for i in range(b):
        batch = ["s%d %d" % (i, j) for j in range(i)]
        if i == 1:
            batch.append("")
        sink(batch)


def produce_hits(b, sink):
    for i in range(b):
        sink([{"name": "h%d%d" % (i, j), "score": i * 10 + j} for j in range(i)])


def show_rows(xs):
    return "rows %d:%s\n" % (len(xs), "".join(" " + ",".join(str(v) for v in r) for r in xs))


def show_strs(xs):
    return "strs %d:%s\n" % (len(xs), "".join(" [%s]" % s for s in xs))


def show_hits(xs):
    return "hits %d:%s\n" % (len(xs), "".join(" %s=%d" % (h["name"], h["score"]) for h in xs))
