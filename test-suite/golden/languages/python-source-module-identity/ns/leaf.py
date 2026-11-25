from .other import tag
import ns.other

def ns_kind(x):
    return "ns:" + tag() + ":" + ns.other.tag() + ":" + __name__
