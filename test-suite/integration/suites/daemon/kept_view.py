import numpy

KEPT = []

def pyKeepView(v):
    KEPT.append(v)
    return 0 if v.flags["OWNDATA"] else 1

def pyKeptTotal(done):
    return float(sum(float(numpy.sum(v)) for v in KEPT))
