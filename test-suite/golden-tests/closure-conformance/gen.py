#!/usr/bin/env python3
"""Generate the main modules and the Makefile.

A program pairs the language-agnostic test (test.loc) with a home instance
(where a counted computation lives) and an away instance (where a value is
applied or forced); the checker is Python. Per language L there are three:
home-L (away Python), away-L (home Python) and local-L (both L). `make`
builds and runs those ten programs and is what the suite compares.

`make cells` is the dredge: one program per (program, cell), so a build
failure costs one cell rather than the whole report. It is slow and is not
part of the suite; run it when hunting.
"""

import os
import re
import sys

LANGS = ["py", "cpp", "r", "rust"]


def programs():
    out = [("local-py", "py", "py")]
    for l in LANGS[1:]:
        out += [(f"home-{l}", l, "py"), (f"away-{l}", "py", l), (f"local-{l}", l, l)]
    return out


def cells():
    src = open("test.loc").read()
    names = re.findall(r'testEqual "\d+ (\w+)"', src)
    args = {}
    for name in names:
        m = re.search(rf"^{name} :: (.*)$", src, re.M)
        args[name] = m.group(1).strip().startswith("Str ->")
    return [(n, args[n]) for n in names]


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    progs = programs()
    for name, home, away in progs:
        with open(os.path.join(here, f"main-{name}.loc"), "w") as f:
            f.write(f"module main (test)\nimport .test (test)\nimport .home-{home}\nimport .away-{away}\n")
    cs = cells()
    if "--cells" in sys.argv:
        for name, home, away in progs:
            for cell, _ in cs:
                with open(os.path.join(here, f"cell-{name}-{cell}.loc"), "w") as f:
                    f.write(f"module main ({cell})\nimport .test ({cell})\nimport .home-{home}\nimport .away-{away}\n")
        return
    with open(os.path.join(here, "Makefile"), "w") as f:
        f.write("PROGRAMS = " + " ".join(n for n, _, _ in progs) + "\n\n")
        f.write("all:\n\t: > obs.txt\n\trm -f *.err cnt-*\n")
        for name, _, _ in progs:
            f.write(f"\techo \"--- {name} ---\" >> obs.txt\n")
            f.write(f"\tmorloc make -o {name} main-{name}.loc 2>> build.err || echo \"BUILD FAILED\" >> obs.txt\n")
            f.write(f"\t[ -x {name} ] && ./{name} test $$PWD/cnt-{name} >> obs.txt 2>> obs.err || true\n")
        f.write("\ncells:\n\tpython3 gen.py --cells\n\t: > cells.txt\n\trm -f cell-*.err cnt-cell-*\n")
        for name, _, _ in progs:
            for cell, takes_path in cs:
                arg = f" $$PWD/cnt-cell-{name}-{cell}" if takes_path else ""
                f.write(f"\t@printf '%-12s %-18s ' {name} {cell} >> cells.txt; "
                        f"if morloc make -o cell-{name}-{cell} cell-{name}-{cell}.loc 2> cell-{name}-{cell}.err; "
                        f"then ./cell-{name}-{cell} {cell}{arg} >> cells.txt 2>&1 || true; "
                        f"else echo BUILD-FAILED >> cells.txt; fi\n")
        f.write("\nclean:\n\trm -rf $(PROGRAMS) __pycache__ *.err cnt-* cell-*\n")
        f.write("\trm -rf *-build *build.tmp*\n")


if __name__ == "__main__":
    main()
