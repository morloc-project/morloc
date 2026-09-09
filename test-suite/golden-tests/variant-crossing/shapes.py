# A payload-bearing `data` value crosses as a structural pair: the
# constructor's name and a tuple of its fields. This is a documented interim
# representation; a native form (a frozen dataclass per arm) needs a
# conversion at the serialization boundary that does not exist yet.
def area(s):
    match s:
        case ("Circle", (r,)):
            return 3.0 * r * r
        case ("Rect", (w, h)):
            return w * h
        case _:
            return 0.0


def grow(s):
    match s:
        case ("Circle", (r,)):
            return ("Circle", (r + 1.0,))
        case ("Rect", (w, h)):
            return ("Rect", (w + 1.0, h + 1.0))
        case _:
            return s
