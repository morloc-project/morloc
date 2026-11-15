import json
import sys

xs = json.load(sys.stdin)
runs = []
for x in xs:
    if x % 100 == 0:
        runs.append([])
    runs[-1].append(x)
whole = sum(r == [r[0] + k for k in range(len(r))] for r in runs)
print(f"{len(xs)} elements, sum {sum(xs)}, {whole} of {len(runs)} batches whole")
