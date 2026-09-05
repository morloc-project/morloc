import json
import sys

doc = json.load(sys.stdin)
for cmd in doc["commands"]:
    for arg in cmd["arguments"]:
        if arg.get("role") == "flag":
            print("%s short=%s long=%s short_reverse=%s long_reverse=%s default=%s"
                  % (cmd["name"], arg["short"], arg["long"],
                     arg["short_reverse"], arg["long_reverse"], arg["default"]))
