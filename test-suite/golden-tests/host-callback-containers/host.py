import os


def tick(path):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)


def _ok(x):
    return "ok " + str(x)


def h_apply(f, n):
    return _ok(f(n))


def h_ops(o, n):
    return _ok(o["get"](n))


def h_wrap(w, n):
    return _ok(w["inner"]["get"](n))


def h_tup(p):
    return _ok(p[0](p[1]))


def h_list(fs, n):
    return _ok(fs[0](n) + fs[1](n))


def h_opt(f, n):
    return "none" if f is None else _ok(f(n))


def h_map(m, n):
    return _ok(m["a"](n))


def h_susp_ops(s, n):
    return _ok(s()["get"](n))


def h_make_ops(k):
    return {"get": lambda i: i + k}


def h_make_list(k):
    return [lambda i: i + k, lambda i: i * k]


def h_cb_ret(mk, n):
    return _ok(mk(n)["get"](n))


def h_cb_param(cb):
    return _ok(cb({"get": lambda i: i * 10}))


def h_chain(c):
    depth = 0
    while c is not None:
        depth += 1
        c = c["next"]
    return "ok " + str(depth)


def h_node(nd, n):
    return _ok(nd["run"](n))


def py_wrap(s):
    return s
