def pyThrow1(x):
    raise ValueError(f"pyThrow1: x={x}")

def pyThrow2(x):
    raise ValueError(f"pyThrow2: x={x}")

def pyThrowList(xs):
    raise ValueError(f"pyThrowList: len={len(xs)}")

def pyThrowTup(t):
    raise ValueError(f"pyThrowTup: t={t}")
