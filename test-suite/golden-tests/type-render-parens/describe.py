import json
import sys

doc = json.load(sys.stdin)
for cmd in doc["commands"]:
    args = ", ".join(a["type"]["morloc"] for a in cmd["arguments"])
    print("%s args=[%s] return=%s" % (cmd["name"], args, cmd["return"]["type"]["morloc"]))
for nt in doc["types"]:
    if nt["kind"] == "data":
        for c in nt["constructors"]:
            print("%s.%s fields=%s" % (nt["name"], c["name"], json.dumps(c["fields"])))
