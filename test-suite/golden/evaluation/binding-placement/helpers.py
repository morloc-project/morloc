import os

_LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), "note.log")


def note(s):
    with open(_LOG, "a") as fh:
        fh.write(s + "\n")
