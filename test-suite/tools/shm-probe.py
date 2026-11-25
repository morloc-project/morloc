#!/usr/bin/env python3
"""Report the shared-memory objects morloc programs hold, on Linux and macOS.

macOS cannot list POSIX shared-memory objects, so the runtime records each
one with a marker file `<name>.shm` in its run directory, /tmp/morloc.XXXXXX,
whose `.owner` file names the nexus that made it. This reads those markers
and checks each named object with shm_open. On Linux it also lists /dev/shm
and fails if an object there has no marker, so the record macOS relies on is
checked wherever the suite runs.

  shm-probe.py count PID     live objects of the run owned by PID
  shm-probe.py size PID      their total size in bytes (page-rounded on macOS)
  shm-probe.py names PID     their names, one per line
  shm-probe.py live FILE     how many names listed in FILE still exist
  shm-probe.py count-all     live objects of every run of this user (and, on
                             Linux, every mlc- object in /dev/shm)
"""

import glob
import os
import sys

import _posixshmem


def run_dirs(owner=None):
    """Run directories of this user, optionally only the one owned by `owner`."""
    for d in glob.glob("/tmp/morloc.??????"):
        try:
            st = os.lstat(d)
            with open(os.path.join(d, ".owner")) as f:
                pid = f.read().split()[0]
        except (OSError, IndexError):
            continue
        if st.st_uid != os.geteuid():
            continue
        if owner is None or pid == str(owner):
            yield d


def marked(dirs):
    names = set()
    for d in dirs:
        for m in glob.glob(os.path.join(d, "mlc-*.shm")):
            names.add("/" + os.path.basename(m)[: -len(".shm")])
    return names


def size_of(name):
    """Size of the object, or None if it does not exist."""
    try:
        fd = _posixshmem.shm_open(name, os.O_RDONLY, mode=0o600)
    except FileNotFoundError:
        return None
    try:
        return os.fstat(fd).st_size
    finally:
        os.close(fd)


def listed(prefix):
    """Objects /dev/shm shows with `prefix`; None where it cannot be listed."""
    if not os.path.isdir("/dev/shm"):
        return None
    return {"/" + n for n in os.listdir("/dev/shm") if n.startswith(prefix)}


def live(names, prefix):
    found = {n: s for n in names if (s := size_of(n)) is not None}
    on_disk = listed(prefix)
    if on_disk is not None:
        unmarked = on_disk - set(names)
        if unmarked:
            sys.exit("shm-probe: objects without markers: " + " ".join(sorted(unmarked)))
    return found


def main():
    cmd = sys.argv[1] if len(sys.argv) > 1 else ""
    if cmd in ("count", "size", "names"):
        pid = int(sys.argv[2])
        found = live(marked(run_dirs(pid)), "mlc-%06x-" % (pid & 0xFFFFFF))
        if cmd == "count":
            print(len(found))
        elif cmd == "size":
            print(sum(found.values()))
        else:
            print("\n".join(sorted(found)))
    elif cmd == "live":
        with open(sys.argv[2]) as f:
            names = [line.strip() for line in f if line.strip()]
        print(sum(1 for n in names if size_of(n) is not None))
    elif cmd == "count-all":
        # Other users' and older runs' objects carry no marker here, so on
        # Linux the listing is counted alongside rather than checked against.
        names = {n for n in marked(run_dirs()) if size_of(n) is not None}
        print(len(names | (listed("mlc-") or set())))
    else:
        sys.exit(__doc__)


main()
