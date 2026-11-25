def shout(s):
    if not isinstance(s, str):
        raise TypeError(f"expected a str, got {type(s).__name__}")
    return s.upper()
