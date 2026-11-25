import numpy

KEPT = []

def pyKeep(v):
    # The array outlives this call: it is read again by pyKeptTotal.
    KEPT.append(v)
    return 0

def pyKeptTotal(done):
    return float(sum(float(numpy.sum(v)) for v in KEPT))

def pyOwnsData(v):
    return str(v.flags["OWNDATA"])

def pyTriplesTotal(vs):
    return float(sum(float(numpy.sum(v)) for v in vs))
