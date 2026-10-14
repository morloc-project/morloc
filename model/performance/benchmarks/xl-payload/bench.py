LANGS = ["py", "cpp", "r", "rs"]

# kind: (sizes, unit of x, unit of the per-x slope)
KINDS = {
    "reals": ([0, 1000000, 2000000, 4000000], "element", "ns"),
    "str": ([0, 16000000, 32000000, 64000000], "byte", "ns"),
    "strs": ([0, 250000, 500000, 1000000], "element", "ns"),
}


def run(b):
    b.make()
    for kind, (xs, x_unit, b_unit) in KINDS.items():
        if b.quick:
            xs = xs[:2]
        slopes = {}
        for src in LANGS:
            for dst in LANGS:
                name = f"{kind}{src.capitalize()}{dst.capitalize()}"
                item = "XL-4" if src == dst else "XL-3"
                _, slope, _ = b.sweep_linear(item, f"{kind}:{src}->{dst}", xs,
                                             lambda n: ["./nexus", name, str(n)],
                                             x_unit, b_unit=b_unit)
                slopes[src, dst] = slope
        # The crossing's own cost per unit: the crossed slope less the
        # producer's in-pool slope, which is the cost of building the value
        # (measuring it is O(1) in every language).
        for src in LANGS:
            for dst in LANGS:
                if src != dst:
                    extra = slopes[src, dst] - slopes[src, src]
                    b.record("XL-3", f"{kind}:{src}->{dst}.cross", extra * 1e9, b_unit,
                             f"per {x_unit}, crossed slope less producer's in-pool slope")
