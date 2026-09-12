import json
import sys

doc = json.load(sys.stdin)
for cmd in doc["commands"]:
    print("%s returns %s" % (cmd["name"], cmd["return"]["type"]["morloc"]))
for nt in doc["types"]:
    head = " ".join([nt["name"]] + nt["parameters"])
    if nt["kind"] == "data":
        body = " | ".join(" ".join([c["name"]] + c["fields"]) for c in nt["constructors"])
    else:
        body = ", ".join("%s :: %s" % (f["key"], f["type"]) for f in nt["fields"])
    print("type %s kind=%s %s" % (head, nt["kind"], body))
