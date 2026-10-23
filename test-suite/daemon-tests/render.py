"""Handlers for the daemon render-selection test (render.loc)."""


def identity(x):
    return x


def shout(x):
    return x.upper()


def whisper(x):
    return x.lower()


def size(x):
    return len(x)
