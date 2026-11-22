def mk(n):
    return [10 * n + i for i in range(n)]


def render_pieces(xs):
    if not xs:
        return []
    return [""] + [f"{x}," for x in xs] + ["\n"]


def render_byte_pieces(xs):
    return [list(s.encode()) for s in render_pieces(xs)]
