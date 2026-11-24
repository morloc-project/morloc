import sys
sys.stdout.write("".join(f"{x}:{i}\n" for n in (2, 3) for x in range(10 * n, 11 * n) for i in range(1000)))
