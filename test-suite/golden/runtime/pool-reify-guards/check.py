# The pools refuse to send a function morloc did not make (HOST-9), the
# last guard behind the compiler's refusal at build. No morloc program
# reaches these guards, so this script and check.R load the Python and R
# pool templates of this repository and call their reify functions
# directly. The Rust guard is tested in rustmorloc (fv.rs).
import sys

src = open(sys.argv[1]).read()
ns = {"__name__": "pool"}
exec(compile(src, sys.argv[1], "exec"), ns)
reify = ns["mlc_reify"]


def refused(f, what):
    try:
        reify(f, "py")
        print("UNEXPECTED: py sent " + what)
    except RuntimeError as e:
        print(("OK: py refuses " if "did not create" in str(e) else "UNEXPECTED: py error for ") + what)


refused(lambda x: x + 1, "a host function")


def __mm_7(x):
    return x


refused(__mm_7, "a host function named like a manifold")

f = lambda x: x
f.__mlc_origin__ = ("cpp", 3, [])
print("OK: py passes an origin back" if reify(f, "py") == ("cpp", 3, []) else "UNEXPECTED: py origin")
