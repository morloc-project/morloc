def show(o):
    return " ".join(f"{k}={o[k]!r}" for k in sorted(o))
