def morloc_packMap (xs):
  d = dict()
  ks, vs = xs
  for (k,v) in zip(ks, vs):
    d[k] = v
  return d

def morloc_unpackMap (d):
  return [list(d.keys()), list(d.values())]

def morloc_person(name):
    return dict(name = name, age = 42)
