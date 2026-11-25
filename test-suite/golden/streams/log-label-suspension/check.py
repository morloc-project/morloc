import re
import sys

runs = int(sys.argv[1])
lines = [l for l in sys.stdin.read().splitlines() if l.startswith("LOG ")]
starts = sum(1 for l in lines if l.endswith(" start"))
times = [float(m.group(1)) for l in lines for m in [re.search(r"done in ([0-9.]+)s$", l)] if m]
print("starts=%d dones=%d total>=%d:%s" % (starts, len(times), runs, sum(times) >= runs))
