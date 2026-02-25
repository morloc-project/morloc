def makePerson(name, age):
    return {"name": name, "age": age}

def getName(p):
    return p["name"]

def getAge(p):
    return p["age"]

def toNull(x):
    return x

def findPerson(name, people):
    for p in people:
        if p["name"] == name:
            return p
    return None
