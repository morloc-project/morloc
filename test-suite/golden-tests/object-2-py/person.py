class PersonObj:
  def __init__(self, name, info):
    self.name = name 
    self.info = info 

def newPerson(name, age):
    return PersonObj(name, age)

def birthday(person):
    person.info += 1
    return person
