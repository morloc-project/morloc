_count = 0

def tick(x):
    global _count
    _count += 1
    return x

def boom(x):
    raise ValueError(f"boom {x}")

def ticks(x):
    return (x, _count)

def bump(x):
    global _count
    _count += 1
    return x
