def checksum(mat):
    # mat is a numpy.ndarray (Matrix m n I32) handed across from Futhark via the
    # C++ pool. Reduce all iteration counts to one deterministic integer so the
    # golden can assert an exact value.
    return int(mat.sum())
