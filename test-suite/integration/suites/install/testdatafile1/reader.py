def readfile(path):
    with open(path) as f:
        return f.read().strip()
