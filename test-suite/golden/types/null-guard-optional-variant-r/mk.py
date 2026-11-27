def in_opt(s):
    return s + chr(0) + s


def in_tag(s):
    from mlc_types._local.main import Tag
    return Tag.Named(s + chr(0) + s)


def in_chain(s, n):
    link = {"label": s + chr(0) + s, "next": None}
    for _ in range(n - 1):
        link = {"label": s, "next": link}
    return link


def clean_opt(s):
    return s + s


def clean_tag(s):
    from mlc_types._local.main import Tag
    return Tag.Named(s + s)
