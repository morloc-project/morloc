# Cross-process-safe observation channel. morloc's Python runtime spawns
# multiple worker processes, so an in-memory tally would live in a
# different process than the reader. Instead, `tap_py` appends each
# invocation's value to a file, and `total_py` reads the file back and
# sums. This survives multi-worker dispatch: any worker's append lands in
# the same on-disk file that any other worker's read observes.

from pathlib import Path

_TAP_LOG = Path("tap_log.txt")

def tap_py(x):
    with _TAP_LOG.open("a") as f:
        f.write(f"{int(x)}\n")

def total_py():
    if not _TAP_LOG.exists():
        return 0
    return sum(int(line) for line in _TAP_LOG.read_text().splitlines() if line.strip())

def reset_py():
    if _TAP_LOG.exists():
        _TAP_LOG.unlink()
