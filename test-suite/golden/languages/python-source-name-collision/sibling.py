import copy
import json
import helper

def sibling_kind(x):
    # `copy` and `json` here must be the standard library, even though
    # sibling source files are named copy.py and json.py: sourced files are
    # registered under reserved keys and never replace a real module. `copy`
    # is already loaded when the sources are, `json` is not, so the two reach
    # the standard library by different routes. `helper` is a plain sibling
    # file with no installed namesake and must still be found beside this one
    return "sibling:" + json.dumps(copy.deepcopy(x)) + ":" + helper.tag()
