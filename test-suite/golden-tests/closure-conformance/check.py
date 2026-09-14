def testEqual(msg, x, y, results):
    (nfails, ntests) = results
    if x == y:
        print(f"  {msg} ... PASS")
        return (nfails, ntests + 1)
    print(f"  {msg} ... FAIL (got {x!r}, expected {y!r})")
    return (nfails + 1, ntests + 1)

def printResult(results):
    (nfails, ntests) = results
    print(f"{ntests - nfails}/{ntests} passed")
    return results
