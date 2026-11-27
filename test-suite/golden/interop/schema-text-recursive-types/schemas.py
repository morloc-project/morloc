import json
m = json.load(open("nexus-build/manifest.json"))
for c in m["commands"]:
    print(c["name"], c["args"][0]["general_schema"])
