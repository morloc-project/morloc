import json
import sys

doc = json.load(sys.stdin)
for cmd in doc["commands"]:
    t = cmd["arguments"][0]["type"]
    print("%s morloc=%s wire=%s" % (cmd["name"], t["morloc"], t["wire"]))
for nt in doc["types"]:
    fields = ", ".join("%s :: %s" % (f["key"], f["type"]) for f in nt["fields"])
    print("type %s kind=%s fields={%s}" % (nt["name"], nt["kind"], fields))
