import json
import sys

doc = json.load(sys.stdin)
for cmd in doc["commands"]:
    for arg in cmd["arguments"]:
        print("name=%s" % arg["name"])
        print("metavar=%s" % arg["metavar"])
        print("description=%s" % ";".join(arg["description"]))
        print("morloc=%s" % arg["type"]["morloc"])
