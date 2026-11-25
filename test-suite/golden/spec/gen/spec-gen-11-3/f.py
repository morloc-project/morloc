def kinds(b):
    best = b["best"]
    return " ".join([type(b).__name__] + [type(p).__name__ for p in b["pts"]] + [type(best).__name__])
