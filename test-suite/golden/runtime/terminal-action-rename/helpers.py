def count_entries(entries):
    # entries arrives as a Python dict from morloc_packMap.
    kept = sum(1 for v in entries.values() if v >= 10)
    return "n={}".format(kept)


def shout(s):
    return s.upper()
