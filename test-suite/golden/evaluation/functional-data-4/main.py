def runAll(x, fs):
    ys = []
    for f in fs:
        ys.append(f(x))
    return ys
