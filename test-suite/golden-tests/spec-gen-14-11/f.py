def mk(n):
    return {"t": n / 10.0, "kids": [mk(n - 1)] if n > 0 else []}

def tens(x):
    return [int(x["t"] * 10.0 + 0.5)] + [v for k in x["kids"] for v in tens(k)]
