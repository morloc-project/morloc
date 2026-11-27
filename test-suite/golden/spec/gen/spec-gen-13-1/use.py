import sys
sys.path.insert(0, sys.argv[1])
from mlc_types._local.main import P
p = P(x=3, name="a")
print(p.x, p.name, p["name"])
