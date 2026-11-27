_state = {"count": 0}


def py_id(x):
    return x


def py_counter():
    _state["count"] += 1
    return _state["count"]
