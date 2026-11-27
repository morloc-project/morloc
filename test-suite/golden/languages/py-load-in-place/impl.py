def show_any(x):
    return repr([v.tolist() if hasattr(v, "tolist") else v for v in x])
