CMDS = ["py", "cpp", "r", "rs", "pyCpp", "all4"]


def run(b):
    b.make()
    reps = 3 if b.quick else 15
    cmd = {c: ["./nexus", c, "1"] for c in CMDS}
    cmd["help"] = ["./nexus", "-h"]
    names = ["help"] + CMDS
    best = dict(zip(names, b.sweep("start", names, lambda c: cmd[c], reps=reps)))
    b.record("START-1", "help", best["help"] * 1e3, "ms", "nexus alone, no pool")
    for c in CMDS:
        b.record("START-2", f"{c}.wall", best[c] * 1e3, "ms")
    singles = [best[c] for c in ("py", "cpp", "r", "rs")]
    b.record("START-3", "all4.wall", best["all4"] * 1e3, "ms",
             f"sum of single-pool starts {sum(singles) * 1e3:.0f} ms, "
             f"largest {max(singles) * 1e3:.0f} ms")
