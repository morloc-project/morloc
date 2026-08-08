"""Pool-side implementations for shapes.loc (MCP test suite)."""


def add_one(x):
    return x + 1


def sum_ints(xs):
    return sum(xs)


def mul2(x, y):
    return x * y


def maybe_double(x):
    # ?Int round-trips through MCP as an integer-or-null.
    return None if x is None else x * 2


def greet_py(name):
    return "Hello, " + name + "!"


def _fmt_config(c):
    # ConfigFlat / ConfigWhole are both Python dicts.
    return "%s:%d:%s" % (c["tmpDir"], c["numThreads"], c["removeCaches"])


def config_str(c):
    return _fmt_config(c)


def config_str2(c):
    return _fmt_config(c)


def mk_result(n):
    # A record return -> MCP structuredContent + object outputSchema.
    return {"status": "ok", "value": n}


def noisy(x):
    import sys
    # This stray stdout write must NOT reach the JSON-RPC protocol stream.
    # The MCP server re-homes fd 1 (aliasing it onto fd 2) BEFORE spawning any
    # pool, so this line lands on stderr where it is harmless.
    print("STRAY-POOL-STDOUT")
    sys.stdout.flush()
    sys.stderr.write("noisy-stderr-marker\n")
    sys.stderr.flush()
    return x + 1
