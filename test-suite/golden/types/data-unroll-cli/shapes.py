def describe(s):
    if s is None:
        return "none"
    return type(s).__name__.lower()
