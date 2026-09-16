import copy

def app_copy_kind(x):
    return "app.copy:" + copy.deepcopy(x) + ":" + __name__
