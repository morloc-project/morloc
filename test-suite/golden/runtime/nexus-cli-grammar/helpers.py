def echoMap(opts, entries):
    # entries arrives as a Python dict (morloc_packMap converts the
    # wire-level list-of-tuples into dict) so iterate .values().
    kept = sum(1 for v in entries.values() if v >= opts["minval"])
    return "{}:{}".format(opts["tag"], kept)


def shout(s):
    return s.upper()


def fhead(n):
    return "fhead:{}".format(n)


def describe(n):
    return "describe:{}".format(n)
