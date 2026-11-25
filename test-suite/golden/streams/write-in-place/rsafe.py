# A batch file R can hold: no NUL in a string (R strings cannot carry one)
# and no -2^63 (bit64's NA).
import json
import sys


def fix(v):
    if isinstance(v, str):
        return v.replace("\0", "")
    if isinstance(v, bool):
        return v
    if isinstance(v, int) and v == -2**63:
        return v + 1
    if isinstance(v, list):
        return [fix(x) for x in v]
    if isinstance(v, dict):
        return {k: fix(x) for k, x in v.items()}
    return v


text = open(sys.argv[1]).read()
if "\\u0000" not in text and str(-2**63) not in text:
    open(sys.argv[2], "w").write(text)
else:
    json.dump(fix(json.loads(text)), open(sys.argv[2], "w"))
