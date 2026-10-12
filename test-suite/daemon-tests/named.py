def shift_by(x, g, neg):
    return -(x + g) if neg else x + g


def opts_summary(o):
    return f"{o['label']}:{o['count']}"
