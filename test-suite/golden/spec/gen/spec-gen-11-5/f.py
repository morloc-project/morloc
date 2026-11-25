from mlc_types._local.main import Person

class Tag:
    def __init__(self, person):
        self.person = person

def tag_pack(p):
    if not isinstance(p, Person):
        raise TypeError(f"expected Person, got {type(p).__name__}")
    return Tag(p)

def tag_unpack(t):
    return t.person

def mkTag(n, a):
    return Tag(Person(name=n, age=a))

def tagName(t):
    return f"{t.person.name}:{t.person.age}:{type(t.person).__name__}"
