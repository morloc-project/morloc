"""Recursion depth: the deepest n each recursion reaches, written in morloc
and written natively, in every language. Ceilings are found by bisection;
a probe succeeds when the program exits 0 and prints n."""

import os
import subprocess

LANGS = ["py", "r", "cpp", "rs"]
# C++ and Rust natives run on a pool worker's stack size, so the ratio
# measures frames, not the difference between worker and main stacks.
WORKER_STACK_KIB = 2048
TAIL_DEPTH = 1_000_000


def ok(cmd, cwd, n):
    argv = [a.replace("{n}", str(n)) for a in cmd]
    try:
        p = subprocess.run(argv, cwd=cwd, capture_output=True, text=True, timeout=600)
    except subprocess.TimeoutExpired:
        return False
    return p.returncode == 0 and p.stdout.strip().strip('"') == str(n)


def ceiling(cmd, cwd, cap):
    """Largest n <= cap that succeeds (cap itself if it does)."""
    lo, n = 0, 16
    while n < cap and ok(cmd, cwd, n):
        lo, n = n, n * 2
    if n >= cap:
        if ok(cmd, cwd, cap):
            return cap
        n = cap
    hi = n
    while hi - lo > 1:
        mid = (lo + hi) // 2
        if ok(cmd, cwd, mid):
            lo = mid
        else:
            hi = mid
    return lo


def native_cmd(lang):
    stack = ["sh", "-c", f"ulimit -s {WORKER_STACK_KIB}; exec \"$0\" \"$@\""]
    return {
        "py": ["python3", "native.py", "{n}"],
        "r": ["Rscript", "native.R", "{n}"],
        "cpp": stack + ["./native-cpp", "{n}"],
        "rs": stack + ["./native-rs", "{n}"],
    }[lang]


def run(b):
    cap = 100_000 if b.quick else 4_000_000
    subprocess.run(["g++", "-O2", "-std=c++17", "-o", "native-cpp", "native.cpp"], cwd=b.dir, check=True)
    subprocess.run(["rustc", "-O", "-o", "native-rs", "native.rs"], cwd=b.dir, check=True)
    for lang in LANGS:
        b.make(loc=f"main-{lang}.loc", out=f"nexus-{lang}")
        prog = f"./nexus-{lang}"
        native = ceiling(native_cmd(lang), b.dir, cap)
        written = ceiling([prog, "morlocNT", "{n}"], b.dir, cap)
        sourced = ceiling([prog, "sourcedNT", "{n}"], b.dir, cap)
        tail = TAIL_DEPTH if ok([prog, "morlocT", "{n}"], b.dir, TAIL_DEPTH) else \
            ceiling([prog, "morlocT", "{n}"], b.dir, TAIL_DEPTH)
        note = f"native {native}, morloc-written {written}, sourced {sourced}"
        b.record("DEPTH-1", f"{lang}.ratio", native / max(written, 1), "x", note)
        b.record("DEPTH-2", f"{lang}.gap", native - sourced, "levels", note)
        b.record("DEPTH-3", f"{lang}.tail", tail, "levels")
