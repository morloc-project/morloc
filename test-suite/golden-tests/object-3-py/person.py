class PersonObj:
  def __init__(self, name, info):
    self.name = name 
    self.info = info 

  def hello(self):
    return "hello from " + self.name
