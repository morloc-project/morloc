"""Generate the OWN-7 move/copy matrix (goldens own7-matrix-cpp and
own7-matrix-rust; run from the golden directory): callee shapes that keep a parameter
on one branch, crossed with the ways a caller can hold its argument.

  python3 gen.py LANG OUTDIR [GROUP]

Writes OUTDIR/main.loc, OUTDIR/cases.tsv (name, shape, predicted copies
under the two-form rule) and OUTDIR/Makefile. Each case prints
`[value, copies]`; the Makefile runs every case built normally and built
with MORLOC_NO_MOVES=1 (the oracle) and check.py compares them.
"""

import itertools
import sys

LANG = "cpp"

# Kinds of parameter: (name, type, make n, read x, default, part x or None)
KINDS = {
    "P": ("Pay", "mkp {n}", "keyp {x}", "mkp 0", None),
    "R": ("R", "{{p = mkp {n}, k = 1}}", "keyp (.p {x}) + .k {x}", "{{p = mkp 0, k = 0}}", ".p {x}"),
    "T": ("(Pay, Int)", "(mkp {n}, 1)", "keyp (.0 {x}) + .1 {x}", "(mkp 0, 0)", ".0 {x}"),
    "L": ("[Pay]", "[mkp {n}, mkp 1]", "sum (map keyp {x})", "[]", "lhead {x}"),
    "D": ("Bx", "Bx (mkp {n}) 1", "readBox {x}", "NoBox", "boxPay {x}"),
    "N": ("N", "{{t = (mkp {n}, 1), w = 2}}", "keyp (.0 (.t {x})) + .1 (.t {x}) + .w {x}", "{{t = (mkp 0, 0), w = 0}}", ".0 (.t {x})"),
}

# Ways a callee keeps its parameter on the branch j > 0: (result type,
# kept expression, default expression, read of the result)
def keep_modes(k):
    t, mk, rd, dflt, part = KINDS[k]
    modes = {
        "ret": (t, "{x}", dflt, rd.replace("{x}", "{r}")),
        "ctor": ("W" + k, "{{w = {x}, tag = 1}}", "{{w = " + dflt + ", tag = 0}}", "(" + rd.format(x="(.w {r})") + ") + .tag {r}"),
        "list": ("[" + t + "]", "[{x}]", "[]", "sum (map (\\v -> " + rd.format(x="v") + ") {r})"),
        "call": ("W" + k, "wrap" + k + " {x}", "{{w = " + dflt + ", tag = 0}}", "(" + rd.format(x="(.w {r})") + ") + .tag {r}"),
    }
    if part is not None:
        modes["part"] = ("Pay", part, "mkp 0", "keyp {r}")
    return modes

# How the callee branches: KEEP and DFLT are filled in, x names the parameter
BRANCHES = {
    "if": "? j > 0 = {keep} : {dflt}",
    "nested": "? j > 1 = {dflt} : ? j > 0 = {keep} : {dflt}",
    "let": "(let y = x in ? j > 0 = {keepy} : {dflt})",
    # recursive, so the callee is a call rather than inlined
    "rec": "? j > 1 = {self} (j - 2) x : ? j > 0 = {keep} : {dflt}",
}

# What the caller passes; READ(e) reads the callee's result, MK makes a
# fresh argument, RK reads an argument. Second field: whether the argument
# may move, or "loop" (a slot passed each of 3 iterations), or "twice".
CALLERS = {
    "tmp": ("{READ} (f j ({MK}))", True),
    "last": ("(let x = {MK} in {READ} (f j x))", True),
    "live": ("(let x = {MK} in {READ} (f j x) + {RK} x)", False),
    "liveSeq": ("(let x = {MK} in let a = {RK} x in a + {READ} (f j x))", True),
    "partLast": ("(let h = {HOLD} ({MK}) in {READ} (f j (.q h)))", True),
    "partLive": ("(let h = {HOLD} ({MK}) in {READ} (f j (.q h)) + {RK} (.q h))", False),
    "param": ("g j ({MK})", True),
    "paramLive": ("gl j ({MK})", False),
    "loop": ("lp j 3 ({MK}) 0", "loop"),
    "loopLast": ("lq j 3 0", True),
    "pap": ("sum (map {READ} (map (f j) [{MK}]))", False),
    "twice": ("(let x = {MK} in {READ} (f2 j x x))", "twice"),
    # kept by a function that keeps it on every path, then read again
    "wrapLive": ("(let x = {MK} in {READW} (wrapK x) + {RK} x + {READ} (f j x))", "wrap"),
    "wrapLast": ("(let x = {MK} in let a = {RK} x in a + {READW} (wrapK x))", "wrapLast"),
    # a part bound by a let, read after the value is passed on
    "aliasAfter": ("(let x = {MK} in let y = {PART} in let r = {READ} (f j x) in r + keyp y)", False),
}


# Payloads a copy of each kind copies; a `data` copy shares its arms.
PAYLOADS = {"P": 1, "R": 1, "T": 1, "L": 2, "D": 0, "N": 1}


def predict(k, mode, caller, j):
    # A part of a list is one element.
    each = 1 if mode == "part" else PAYLOADS[k]
    # A part a function returns (lhead, boxPay) is a copy of it: a function
    # returns a value, not a reference into its argument (OWN-8).
    returned = 1 if caller == "aliasAfter" and k in ("L", "D") else 0
    return each * predict_one(caller, j) + returned


def predict_one(caller, j):
    move = CALLERS[caller][1]
    if move == "wrap":
        return 1 + (1 if j else 0)
    if move == "wrapLast":
        return 0
    if move == "twice":
        return 1
    if j == 0:
        return 0
    if move == "loop":
        return 3
    return 0 if move else 1


def cases():
    out = []
    for k in KINDS:
        for mode in keep_modes(k):
            for br in BRANCHES:
                for caller in CALLERS:
                    if caller == "aliasAfter" and KINDS[k][4] is None:
                        continue
                    for j in (1, 0):
                        out.append((k, mode, br, caller, j))
    return out


def select(all_cases):
    """Every caller with every kind and both branches for the plain shape;
    every keep mode and branch with a spread of callers."""
    picked = []
    for (k, mode, br, caller, j) in all_cases:
        plain = mode == "ret" and br == "if"
        spread = br == "if" and caller in ("tmp", "last", "live", "partLive", "param", "loop", "wrapLive", "wrapLast", "aliasAfter") and j == 1
        recursive = br == "rec" and caller in ("tmp", "last", "live", "partLive", "param", "loop", "twice", "wrapLive", "aliasAfter") and (j == 1 or caller in ("tmp", "live", "aliasAfter"))
        branchy = k == "R" and mode == "ret" and br != "rec"
        if plain or spread or branchy or recursive:
            picked.append((k, mode, br, caller, j))
    return list(dict.fromkeys(picked))


def fmt(s, **kw):
    for a, b in kw.items():
        s = s.replace("{" + a + "}", b)
    return s.replace("{{", "{").replace("}}", "}")


def render(selected):
    lines = []
    tsv = []
    names = []
    for i, (k, mode, br, caller, j) in enumerate(selected):
        name = "c%03d" % i
        names.append(name)
        t, mk, rd, dflt, part = KINDS[k]
        rtype, keep, mdflt, rread = keep_modes(k)[mode]
        body = BRANCHES[br]
        keepx = keep.replace("{x}", "x")
        keepy = keep.replace("{x}", "y")
        body = body.replace("{keep}", keepx).replace("{keepy}", keepy).replace("{dflt}", mdflt).replace("{self}", name + "f")
        body = fmt(body)
        f = name + "f"
        lines.append("-- %s: %s kept by %s under %s, caller %s, j=%d" % (name, k, mode, br, caller, j))
        lines.append("%s :: Int -> %s -> %s" % (f, t, rtype))
        lines.append("%s j x = %s" % (f, body))
        read = "(\\r -> " + fmt(rread, r="r") + ")"
        mkn = fmt(mk, n="n")
        readk = "(\\x -> " + fmt(rd, x="x") + ")"
        call = CALLERS[caller][0]
        readw = "(\\r -> " + fmt("(" + rd.format(x="(.w {r})") + ") + .tag {r}", r="r") + ")"
        call = call.replace("{READW}", readw).replace("wrapK", "wrap" + k)
        call = call.replace("{READ}", read).replace("{MK}", mkn).replace("{RK}", readk).replace("{HOLD}", "hold" + k).replace("{PART}", (part or "").replace("{x}", "x"))
        call = call.replace("f2 j", name + "f2 j").replace("f j", f + " j")
        call = call.replace("gl j", name + "gl j").replace("g j", name + "g j")
        call = call.replace("lp j", name + "lp j").replace("lq j", name + "lq j")
        call = fmt(call)
        if caller == "twice":
            keepa = keep.replace("{x}", "a")
            keepb = keep.replace("{x}", "b")
            lines.append("%sf2 :: Int -> %s -> %s -> %s" % (name, t, t, rtype))
            lines.append("%sf2 j a b = %s" % (name, fmt("? j > 0 = " + keepa + " : " + keepb)))
        if caller == "param":
            lines.append("%sg :: Int -> %s -> Int" % (name, t))
            lines.append("%sg j x = %s (%s j x)" % (name, read, f))
        if caller == "paramLive":
            lines.append("%sgl :: Int -> %s -> Int" % (name, t))
            lines.append("%sgl j x = %s (%s j x) + %s x" % (name, read, f, readk))
        if caller == "loop":
            lines.append("%slp :: Int -> Int -> %s -> Int -> Int" % (name, t))
            lines.append("%slp j k x acc" % name)
            lines.append("  ? k == 0 = acc + %s x" % readk)
            lines.append("  : %slp j (k - 1) x (acc + %s (%s j x))" % (name, read, f))
        if caller == "loopLast":
            lines.append("%slq :: Int -> Int -> Int -> Int" % name)
            lines.append("%slq j k acc" % name)
            lines.append("  ? k == 0 = acc")
            lines.append("  : %slq j (k - 1) (acc + %s (%s j %s))" % (name, read, f, "(" + fmt(mk, n="k") + ")"))
        if caller.startswith("part"):
            call = call.replace("{q = ", "{q = ")
        lines.append("%s :: Int -> Int -> (Int, Int)" % name)
        lines.append("%s n j = census (%s)" % (name, call))
        lines.append("")
        tsv.append("%s\t%s\t%s\t%s\t%s\t%d\t%d" % (name, k, mode, br, caller, j, predict(k, mode, caller, j)))
    return lines, tsv, names


def header(names):
    imp = "root-cpp" if LANG == "cpp" else "root-rust"
    src = "pay.hpp" if LANG == "cpp" else "pay.rs"
    tl = "Cpp" if LANG == "cpp" else "Rust"
    holders = "\n".join(
        "record H%s where\n  q :: %s\n  z :: Int\n\nhold%s :: %s -> H%s\nhold%s v = {q = v, z = 1}\n" % (k, KINDS[k][0], k, KINDS[k][0], k, k) for k in KINDS
    )
    wraps = "\n".join(
        "record W%s where\n  w :: %s\n  tag :: Int\n\nwrap%s :: %s -> W%s\nwrap%s y = {w = y, tag = 2}\n"
        % (k, KINDS[k][0], k, KINDS[k][0], k, k) for k in KINDS
    )
    return """-- Generated by own7-matrix-cpp/gen.py: the OWN-7 move/copy matrix (%s). Each case prints
-- [value, payloads copied].
module main (%s)

import root
import %s

newtype Pay = Int
type %s => Pay = "Pay"

instance Packable Int Pay where
  source %s from "%s" ("pay_pack" as pack, "pay_unpack" as unpack)

source %s from "%s" ("mkp", "keyp", "census")

mkp :: Int -> Pay
keyp :: Pay -> Int
census :: Int -> (Int, Int)

record R where
  p :: Pay
  k :: Int

record N where
  t :: (Pay, Int)
  w :: Int

data Bx = Bx Pay Int | NoBox

readBox :: Bx -> Int
readBox b = match b
  | (Bx q k) = keyp q + k
  | NoBox = 0

boxPay :: Bx -> Pay
boxPay b = match b
  | (Bx q _) = q
  | NoBox = mkp 0

lhead :: [Pay] -> Pay
lhead xs = match xs
  | [a, _] = a
  | _ = mkp 0

%s
%s
""" % (LANG, ", ".join(names), imp, tl, tl, src, tl, src, holders, wraps)


def write_makefile(out, names, js):
    """The Makefile: every case built normally, built with MORLOC_NO_MOVES=1
    (the oracle) and, for C++, under sanitizers, compared by check.py."""
    env = "MORLOC_LANG_PARAMS=rust:lto=off " if LANG == "rust" else ""
    mk = ["all:", "\trm -f *.err obs.txt run.txt oracle.txt asan.txt",
          "\t%smorloc make -o nexus main.loc 2> build.err" % env,
          "\t%sMORLOC_NO_MOVES=1 morloc make -o oracle main.loc 2> oracle-build.err" % env]
    if LANG == "cpp":
        mk.append("\tCXX=\"$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all\" morloc make -o asan main.loc 2> asan-build.err")
    for n in names:
        mk.append("\techo \"%s $$(./nexus %s 7 %d 2>> obs.err || echo failed)\" >> run.txt" % (n, n, js[n]))
        mk.append("\techo \"%s $$(./oracle %s 7 %d 2>> obs.err || echo failed)\" >> oracle.txt" % (n, n, js[n]))
        if LANG == "cpp":
            mk.append("\techo \"%s $$(ASAN_OPTIONS=detect_leaks=0 ./asan %s 7 %d 2>> obs.err || echo failed)\" >> asan.txt" % (n, n, js[n]))
    mk.append("\tpython3 check.py run.txt oracle.txt %s> obs.txt" % ("asan.txt " if LANG == "cpp" else ""))
    mk += ["", "clean:", "\trm -rf nexus oracle asan pools __pycache__ *-build *build.tmp*", "\trm -f *.err run.txt oracle.txt asan.txt"]
    open(out + "/Makefile", "w").write("\n".join(mk) + "\n")


def main():
    global LANG
    LANG = sys.argv[1]
    out = sys.argv[2]
    group = sys.argv[3] if len(sys.argv) > 3 else "all"
    sel = select(cases())
    if group != "all":
        a, b = map(int, group.split(":"))
        sel = sel[a:b]
    lines, tsv, names = render(sel)
    js = {"c%03d" % i: c[4] for i, c in enumerate(sel)}
    open(out + "/main.loc", "w").write(header(names) + "\n".join(lines))
    open(out + "/cases.tsv", "w").write("\n".join(tsv) + "\n")
    write_makefile(out, names, js)
    print(len(names), "cases")


if __name__ == "__main__":
    main()
