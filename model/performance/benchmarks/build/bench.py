"""`morloc make` time for the startup bench's identity functions, one
program per set of pool languages."""

import os
import shutil
import tempfile

SRC = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "startup")
LANGS = {"py": ("Py", "root-py", "f.py"), "cpp": ("Cpp", "root-cpp", "f.hpp"),
         "r": ("R", "root-r", "f.R"), "rs": ("Rust", "root-rust", "f.rs")}
SETS = [["py"], ["cpp"], ["r"], ["rs"], ["cpp", "rs"], ["py", "cpp", "r", "rs"]]


def program(langs):
    lines = ["module main (g)", ""]
    lines += [f"import {LANGS[l][1]}" for l in langs] + [""]
    for l in langs:
        lang, _, f = LANGS[l]
        lines.append(f'source {lang} from "{f}" ("ident" as id{l.capitalize()})')
        lines.append(f"id{l.capitalize()} :: Int -> Int")
    body = "x"
    for l in langs:
        body = f"id{l.capitalize()} ({body})"
    return "\n".join(lines + ["", f"g x = {body}", ""])


def run(b):
    tmp = tempfile.mkdtemp(prefix="perf-build-")
    try:
        dirs = {}
        for langs in SETS[:2] if b.quick else SETS:
            name = "+".join(langs)
            d = dirs[name] = os.path.join(tmp, name)
            os.makedirs(d)
            for l in langs:
                shutil.copy(os.path.join(SRC, LANGS[l][2]), d)
            with open(os.path.join(d, "main.loc"), "w") as h:
                h.write(program(langs))
        names = list(dirs)
        best = b.sweep("make", names, lambda n: ["morloc", "make", "-o", "nexus", "main.loc"],
                       reps=3, cwd_of=lambda n: dirs[n])
        for name, t in zip(names, best):
            b.record("BUILD-1", f"{name}.wall", t, "s", "best of 3, warm caches")
    finally:
        shutil.rmtree(tmp)
