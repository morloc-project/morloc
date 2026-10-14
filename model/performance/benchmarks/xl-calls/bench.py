LANGS = ["py", "cpp", "r", "rs"]

# Loop lengths; chosen so the longest point takes roughly a second.
CROSS = [0, 2000, 4000, 8000]
SAME = {
    "py": [0, 100000, 200000, 400000],
    "r": [0, 20000, 40000, 80000],
    "cpp": [0, 1000000, 2000000, 4000000],
    "rs": [0, 1000000, 2000000, 4000000],
}


def run(b):
    b.make()

    def cut(xs):
        return xs[:2] if b.quick else xs

    for outer in LANGS:
        for inner in LANGS:
            name = outer + inner.capitalize()
            if outer == inner:
                b.sweep_linear("XL-2", f"{outer}->{inner}", cut(SAME[outer]),
                               lambda n: ["./nexus", name, str(n)], "call",
                               b_unit="ns")
            else:
                b.sweep_linear("XL-1", f"{outer}->{inner}", cut(CROSS),
                               lambda n: ["./nexus", name, str(n)], "call")
    for lang in LANGS:
        name = "native" + lang.capitalize()
        b.sweep_linear("XL-2", f"native-{lang}", cut(SAME[lang]),
                       lambda n: ["./nexus", name, str(n)], "call", b_unit="ns")
