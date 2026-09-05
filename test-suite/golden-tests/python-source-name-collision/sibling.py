import copy

def sibling_kind(x):
    # `copy` here must be the standard library, even though a sibling source
    # file is named copy.py: sourced files are registered under reserved keys
    # and never replace a real module
    return "sibling:" + copy.deepcopy(x)
