def producePy(path, sink):
    batch = []
    with open(path) as fh:
        for line in fh:
            batch.append(int(line))
            if len(batch) == 3:
                sink(batch)
                batch = []
    if batch:
        sink(batch)
