import os



def home_tick(path):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)


def home_ident(x):
    return x


def home_mark(s):
    return s


def home_str_len(s):
    return len(s)


def home_add(x, y):
    return x + y


def home_fail(path):
    raise RuntimeError("failHome ran: " + path)


def home_mk(k):
    return lambda x: x + k


def home_big(n):
    return list(range(n))


def home_use_pure(f, x):
    return f(x)


def home_take_thunk(t):
    t()
    t()
    return t()



def away_ident(x):
    return x


def away_bump(path, x):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)


def away_take_thunk(t):
    t()
    t()
    return t()


def away_drop_thunk(t):
    return 0


def away_use_record(h):
    h["run"]()
    h["run"]()
    return h["run"]() + h["tag"]


def away_use_list(xs):
    xs[0]()
    xs[0]()
    return xs[0]()


def away_use_tuple(t):
    t[0]()
    t[0]()
    return t[0]() + t[1]


def away_use_opt(t):
    if t is None:
        return 0
    t()
    t()
    return t()


def away_use_nested(hs):
    hs[0]["run"]()
    hs[0]["run"]()
    return hs[0]["run"]()


def away_use_callback(f, x):
    f(x)
    f(x)
    return f(x)


def away_use_pure(f, x):
    return f(x)


def away_use_arity2(f, x, y):
    return f(x, y)


def away_use_pinned(c, x):
    c["inc"](x)
    c["inc"](x)
    return c["inc"](x)


def away_use_hof(f, g):
    return f(g)


def away_use_list_pure(fs, x):
    return sum(f(x) for f in fs)


def away_use_mk(f, a, b):
    return f(a)(b)


def away_use_thunk2(t):
    inner = t()
    inner()
    inner()
    return inner()


def away_use_thunk_fn(t, x):
    return t()(x)


def away_use_fn_thunk(f, t):
    return f(t)


def away_pass_through(f):
    return f


def away_map_away(f, xs):
    return [f(x) for x in xs]


def away_use_hof_eff(f, g):
    return f(g)


def away_use_hof2(f):
    return f(lambda g: g(3))


def away_two(a, b, x):
    with open(a, "ab") as fh:
        fh.write(b"x")
    return os.path.getsize(a) + len(b)


def away_mix(x, a):
    with open(a, "ab") as fh:
        fh.write(b"x")
    return os.path.getsize(a) + x
