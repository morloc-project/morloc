def pywrap(strLen, arrLen, f):
    seqs = ["x" * strLen for i in range(arrLen)]

    total = 0
    for seq in seqs:
        total += f(seq)

    return total
