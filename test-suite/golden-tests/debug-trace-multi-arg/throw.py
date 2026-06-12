def pyDivIfNonzero(a, b, label):
    """Divides a by b; raises ValueError with the label if b == 0.
    The label arg gives us a 3rd distinct payload for the debug-trace
    catch block to dump."""
    if b == 0:
        raise ValueError(f"pyDivIfNonzero failed ({label}): b must be non-zero")
    return a / b
