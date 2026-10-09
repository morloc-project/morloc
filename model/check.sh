#!/usr/bin/env bash
# Checks the specs in model/ against each other and against their tests.
#   check.sh [--only FILE] [REPO_ROOT]
# FILE is a spec file relative to model/ (language/aliases.md); only its
# problems are reported and counted.
set -euo pipefail
only=""
if [ "${1:-}" = "--only" ]; then only=$2; shift 2; fi
root=${1:-$(cd "$(dirname "$0")/.." && pwd)}

python3 - "$root" "$only" <<'PY'
import os, re, sys
root = sys.argv[1]
only = sys.argv[2]
model = os.path.join(root, "model")
unit = os.path.join(root, "test-suite", "SpecTests.hs")
golden = os.path.join(root, "test-suite", "golden-tests")

errors = []
warnings = []
items = {}
prefix_file = {}
cited = {}

def spec_files(sub):
    base = os.path.join(model, sub)
    for d, dirs, files in os.walk(base):
        dirs[:] = sorted(x for x in dirs if x != "tla")
        for fn in sorted(files):
            if fn.endswith(".md") and fn != "README.md":
                yield os.path.relpath(os.path.join(d, fn), model)

def read(rel):
    path = os.path.join(model, rel)
    raw = open(path, "rb").read()
    for n, line in enumerate(raw.split(b"\n"), 1):
        if any(b > 0x7E or (b < 0x20 and b != 0x09) for b in line):
            errors.append(f"{rel}:{n}: non-ASCII character")
    return raw.decode("utf-8", "replace").split("\n")

def define(rel, n, prefix, num):
    item = f"{prefix}-{num}"
    if item in items:
        errors.append(f"{rel}:{n}: {item} also defined at {items[item]['where']}")
    owner = prefix_file.setdefault(prefix, rel)
    if owner != rel:
        errors.append(f"{rel}:{n}: prefix {prefix} belongs to {owner}")
    items[item] = {"intent": None, "code": None, "tests": [],
                   "where": f"{rel}:{n}", "full": False}
    return item

for rel in spec_files("runtime"):
    for n, line in enumerate(read(rel), 1):
        m = re.match(r"### ([A-Z]+)-(\d+) ", line)
        if m:
            define(rel, n, m[1], m[2])

for rel in [*spec_files("language"), *spec_files("compiler")]:
    item = None
    for n, line in enumerate(read(rel), 1):
        m = re.match(r"### ([A-Z]+)-(\d+) ", line)
        if m:
            item = define(rel, n, m[1], m[2])
            items[item]["full"] = True
            continue
        if item is None:
            continue
        d = items[item]
        if line.startswith("Intent:"):
            if not re.fullmatch(r"Intent: (ruled \d{4}-\d\d-\d\d|proposed|open|retired \d{4}-\d\d-\d\d)", line):
                errors.append(f"{rel}:{n}: malformed Intent")
            d["intent"] = line.split()[1]
        elif line.startswith("Code:"):
            if not re.fullmatch(r"Code: (unaudited|conforms \d{4}-\d\d-\d\d|deviates \S.*)", line):
                errors.append(f"{rel}:{n}: malformed Code")
            d["code"] = line
        elif line.startswith("Tests:"):
            for t in line[len("Tests:"):].split(","):
                t = t.strip()
                d["tests"].append(t)
                if t in cited:
                    errors.append(f"{rel}:{n}: {t} cited by {cited[t]} and {item}")
                cited[t] = item

# Chapters listed under "## Complete chapters" in language/README.md: there a
# ruled item must cite tests and conform. Elsewhere those are warnings.
complete = set()
readme = os.path.join(model, "language", "README.md")
if os.path.exists(readme):
    section = None
    for line in open(readme).read().split("\n"):
        if line.startswith("## "):
            section = line[3:].strip()
        elif section == "Complete chapters":
            m = re.match(r"- `([^`]+\.md)`", line)
            if m:
                complete.add(os.path.join("language", m[1]))

unit_tests = set(re.findall(r'"(spec-[a-z]+-\d+-\d+)"', open(unit).read())) if os.path.exists(unit) else set()
golden_tests = {d for d in os.listdir(golden) if d.startswith("spec-")} if os.path.isdir(golden) else set()
for t in sorted(unit_tests & golden_tests):
    errors.append(f"{t} is both a unit and a golden test")
existing = unit_tests | golden_tests

for item, d in items.items():
    if not d["full"]:
        continue
    if d["intent"] is None:
        errors.append(f"{d['where']}: {item} has no Intent")
    if d["code"] is None:
        errors.append(f"{d['where']}: {item} has no Code")
    strict = d["where"].split(":")[0] in complete
    gaps = errors if strict else warnings
    if d["intent"] == "ruled" and not d["tests"]:
        gaps.append(f"{d['where']}: {item} is ruled but cites no test")
    if d["intent"] == "ruled" and d["code"] and not d["code"].startswith("Code: conforms"):
        gaps.append(f"{d['where']}: {item} is ruled but does not conform")
    if d["intent"] in ("open", "retired") and d["tests"]:
        errors.append(f"{d['where']}: {item} is {d['intent']} but cites tests")
    prefix, num = item.split("-")
    for t in d["tests"]:
        if not re.fullmatch(rf"spec-{prefix.lower()}-{num}-\d+", t):
            errors.append(f"{d['where']}: {t} is not named for {item}")
        if t not in existing:
            errors.append(f"{d['where']}: {t} does not exist")
for t in sorted(existing - set(cited)):
    errors.append(f"{t} exists but no item cites it")

if only:
    errors = [e for e in errors if e.startswith(only + ":")]
    warnings = [w for w in warnings if w.startswith(only + ":")]
for e in errors:
    print(e)
for w in warnings:
    print("warning: " + w)
full = [d for d in items.values() if d["full"]]
counts = {k: sum(1 for d in full if d["intent"] == k) for k in ("ruled", "proposed", "open", "retired")}
print(f"{len(full)} items ({', '.join(f'{v} {k}' for k, v in counts.items())}), "
      f"{len(items) - len(full)} runtime IDs, {len(existing)} tests, "
      f"{len(errors)} problems, {len(warnings)} warnings")
sys.exit(1 if errors else 0)
PY
