# A mapped form in Python is a hint on the wire and nothing more: the value
# is the structural (constructor, fields) pair whatever the mapping says.
def grow(b):
    match b:
        case ("Full", (x,)):
            return ("Full", (x + 100,))
        case _:
            return b


def bump(w):
    return {"item": w["item"] + 100}
