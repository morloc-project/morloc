#!/usr/bin/env python3
"""Reject shell idioms that behave differently on macOS than on Linux.

Development and most testing happen on Linux, so a GNU-only flag or a Linux
path in a test passes here and fails on macOS -- or, worse, silently checks
nothing there. This scans the shell suites, the golden tests' Makefiles and
scripts, and the language setup scripts `morloc init` runs, and fails on any
line matching a known divergence. A line that is deliberately platform-gated
ends with a comment `# portable: <why>`.
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

SCAN = ["test-suite", "data/lang", "data/misc"]
SKIP_DIRS = {"__pycache__", ".git", "pools", "shims", "portability-lint"}

# (pattern, which files, what to do instead)
RULES = [
    (r"\bstat\s+-c\b", "any", "BSD stat has no -c; use `wc -c <` or python"),
    (r"--ppid\b", "any", "BSD ps has no --ppid; use `pgrep -P`"),
    (r"/dev/shm", "any", "macOS has no /dev/shm; use test-suite/shm-probe.py"),
    (r"/proc/", "any", "macOS has no /proc; use ps, pgrep or lsof"),
    (r"\bsed\s+-i\b", "any", "BSD sed -i takes a suffix argument; write to a file and mv"),
    (r"\breadlink\s+-f\b", "any", "readlink -f needs macOS 12.3+; use cd + pwd -P"),
    (r"\bgrep\s+-[A-Za-z]*P", "any", "BSD grep has no -P; use -E"),
    (r"\bfind\b.*-printf\b", "any", "BSD find has no -printf"),
    (r"date\s+\+%[A-Za-z%]*N(?!.*=\s*\"N\")", "any", "BSD date prints a literal N for %N"),
    (r"EPOCHREALTIME|\bmapfile\b|\breadarray\b|\bdeclare\s+-A\b", "any", "bash 4+ only; macOS has bash 3.2"),
    (r"\$\{[A-Za-z_]\w*(,,|\^\^)\}", "any", "bash 4+ case conversion; macOS has bash 3.2"),
    (r"\becho\s+-n\b", "make", "macOS /bin/sh prints -n literally; use printf"),
    (r"\bwc\s+-[lcw]\b[^|;)]*>>", "any", "BSD wc pads its count; pipe through `tr -d ' '`"),
    (r"\"[^\"]*\$+\((?![^)]*tr -d)[^)]*\bwc\s+-[lcw]\b[^)]*\)", "any", "BSD wc pads its count; pipe through `tr -d ' '`"),
    (r"(?<![+{])\"\$\{[A-Za-z_]\w*\[@\]\}\"", "sh", "an empty array is unbound under set -u in bash 3.2; use ${a[@]+\"${a[@]}\"}"),
]


def kind(path):
    name = os.path.basename(path)
    if name == "Makefile" or name.endswith(".mk"):
        return "make"
    if name.endswith(".sh"):
        return "sh"
    return None


def files():
    for top in SCAN:
        for d, dirs, names in os.walk(os.path.join(ROOT, top)):
            dirs[:] = [x for x in dirs if x not in SKIP_DIRS and not x.endswith("-build") and not x.startswith(".")]
            for n in names:
                p = os.path.join(d, n)
                if kind(p):
                    yield p


def main():
    problems = []
    for path in files():
        k = kind(path)
        with open(path, errors="replace") as f:
            for no, line in enumerate(f, 1):
                code = line.rstrip("\n")
                if "# portable:" in code:
                    continue
                stripped = code.lstrip()
                if stripped.startswith("#") or stripped.startswith("@#"):
                    continue
                for pat, where, advice in RULES:
                    if where not in ("any", k):
                        continue
                    if re.search(pat, code):
                        problems.append(f"{os.path.relpath(path, ROOT)}:{no}: {advice}\n    {stripped}")
    if problems:
        print("\n".join(problems))
        print(f"\n{len(problems)} non-portable line(s)")
        return 1
    print("portability lint: clean")
    return 0


sys.exit(main())
