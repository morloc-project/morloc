def addShit(name, level_of_shit, foo):
    shitlists = dict(foo["shitlists"])
    shitlists[name] = {"yolo" : level_of_shit}
    return {**foo, "shitlists": shitlists}
