# Seeded batches of every type main.loc writes, one JSON file per type.
import json
import random

r = random.Random(7)
SIZES = [0, 1, 3, 40, 700, 0, 2]

def real(): return r.choice([0.0, -0.0, 1.5, r.uniform(-1e9, 1e9)])
def i8(): return r.randint(-128, 127)
def i16(): return r.randint(-32768, 32767)
def i32(): return r.randint(-2**31, 2**31 - 1)
def i64(): return r.choice([0, -1, 2**62, -2**63, r.randint(-2**63, 2**63 - 1)])
def small(): return r.choice([0, -1, 1, r.randint(-2**31, 2**31 - 1)])
def string():
    k = r.random()
    if k < 0.01: return "s" * 5000
    if k < 0.1: return ""
    if k < 0.2: return "\u00e9\u4e2d" * r.randint(1, 5)
    if k < 0.25: return "a\u0000b"
    return "x" * r.randint(1, 30)
def lst(f, n=8): return [f() for _ in range(r.randint(0, n))]
def opt(f): return None if r.random() < 0.3 else f()
def shape():
    k = r.randint(0, 2)
    if k == 0: return "Dot"
    if k == 1: return {"Circle": [real()]}
    return {"Box": [{"x": real(), "label": string()}]}
def tree(d):
    if d == 0 or r.random() < 0.3: return "Leaf"
    return {"Node": [small(), tree(d - 1), tree(d - 1)]}
def rose(d):
    return {"Rose": [i32(), [rose(d - 1) for _ in range(r.randint(0, 3))] if d > 0 else []]}
def chain(d):
    if d == 0 or r.random() < 0.2: return "End"
    return {"Link": [string(), chain(d - 1), lst(i8), string()]}
def deep_rose(n):
    # Built as text: json.dumps recurses once per level.
    return "".join(f'{{"Rose":[{i},[' for i in range(n)) + '{"Rose":[0,[]]}' + "]]}" * n

gens = {
    "Real": real, "I8": i8, "Bool": lambda: r.random() < 0.5,
    "Pair": lambda: [i32(), r.random() < 0.5], "I64": i64, "Int": small,
    "Str": string, "Reals": lambda: lst(real), "I8s": lambda: lst(i8),
    "U8s": lambda: lst(lambda: r.randint(0, 255), 80),
    "Strs3": lambda: [lst(string, 3) for _ in range(r.randint(0, 3))],
    "Strs": lambda: lst(string, 6),
    "U8ss": lambda: lst(lambda: lst(lambda: r.randint(0, 255), 80), 5),
    "Realss": lambda: lst(lambda: lst(real), 4),
    "Bools": lambda: lst(lambda: r.random() < 0.5, 80),
    "Boolss": lambda: lst(lambda: lst(lambda: r.random() < 0.5, 70), 4),
    "OptStr": lambda: opt(string), "OptReals": lambda: opt(lambda: lst(real)),
    "OptI16s": lambda: lst(lambda: opt(i16)),
    "Dir": lambda: r.choice(["North", "South", "East"]), "Shape": shape,
    "Two": lambda: {"name": string(), "tag": r.randint(0, 255), "xs": lst(i16), "more": lst(string, 4)},
    "Mixed": lambda: [string(), lst(real), opt(string), i8()],
    "Tree": lambda: tree(5), "Rose": lambda: rose(3),
    "Chain": lambda: chain(6),
}
for name, g in gens.items():
    batches = [[g() for _ in range(n)] for n in SIZES]
    if name in ("Bool", "I8", "Int"):
        batches.append([g() for _ in range(10000)])
    text = json.dumps(batches)
    if name == "Rose":
        text = text[:-1] + ", [" + json.dumps(rose(2)) + ", " + deep_rose(10000) + ", " + json.dumps(rose(2)) + "]]"
    open(f"{name}.json", "w").write(text)
