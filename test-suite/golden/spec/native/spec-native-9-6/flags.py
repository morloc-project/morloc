def writable(xs):
    return "writable" if xs.flags.writeable else "read-only"
