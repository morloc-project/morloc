import mlc_types._local.main as m
from mlc_types._local.main import B, E, V, Wrap


def fa(a):
    named = 0 if m.Outer.__annotations__["b"] == "B" else 100
    return a["x"] + B(y=2).y + (1 if V.Held(B(y=0)) != V.Plain and E.Green != E.Red else 0) + named
