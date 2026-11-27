def base(s):
    return s.encode("utf-8")

def step(s):
    if not isinstance(s, str):
        raise TypeError(f"expected a str, got {type(s).__name__}")
    return (s + "b").encode("utf-8")
