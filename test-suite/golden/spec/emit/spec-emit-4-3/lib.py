def step(k, i):
    if i > k:
        raise ValueError("an operand after the deciding one ran")
    return i != k


def hit(k, i):
    if i > k:
        raise ValueError("an operand after the deciding one ran")
    return i == k
