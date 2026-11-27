def make_nul():
    # Construct "abc\0def\0!" without writing a literal NUL in source.
    return "abc" + chr(0) + "def" + chr(0) + "!"


def echo(s):
    return s


def slen(s):
    return len(s)
