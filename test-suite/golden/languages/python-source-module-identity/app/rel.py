from .util import tag
from . import util

def rel_kind(x):
    return "rel:" + tag() + ":" + util.tag() + ":" + __name__
