def addShit(name, level_of_shit, foo):
    shitlist = dict(foo["shitlist"])
    shitlist[name] = level_of_shit
    return {**foo, "shitlist": shitlist}
