"""Handlers for the daemon render-selection test (render.loc)."""


def identity(x):
    return x


def shout(x):
    return x.upper()


def whisper(x):
    return x.lower()


def size(x):
    return len(x)


def mk(n):
    return [10 * n + i for i in range(n)]


def render_pieces(xs):
    return [f"{x}," for x in xs] + ["\n"]
