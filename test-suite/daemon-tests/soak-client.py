#!/usr/bin/env python3
"""Drive a morloc daemon the way a long-running application would.

Requests ride persistent connections rather than a fresh process per call.
That is how a real client talks to the daemon, it is the only way the
keep-alive path gets exercised at all, and it makes a few thousand requests
cheap enough to run in CI.

Every response is checked against a value derived from that request's own
argument, so a worker thread handing a result to the wrong caller is a
failure rather than a coincidence that happens to look right.

A pool crash is a legitimate thing to meet mid-run: the daemon answers 503
while it rebuilds, and a connection open at that moment can be dropped. Both
are retried. What is never tolerated is a wrong answer, or a request that
never succeeds at all.

Usage: soak-client.py <port> <mode> [options]
  mode `seq`  -- one connection, --requests calls in series
  mode `conc` -- --workers connections in parallel, --requests calls each

Prints one summary line of `key=value` pairs and exits non-zero if any
request failed to complete or returned the wrong value.
"""
import argparse
import http.client
import json
import sys
import threading

RETRY_LIMIT = 60
RETRY_SLEEP = 0.25


class Client:
    """One connection's worth of work, reconnecting as needed."""

    def __init__(self, port, worker_id):
        self.port = port
        self.worker_id = worker_id
        self.conn = None
        self.deferred = 0   # 503s while the daemon rebuilt its pools
        self.dropped = 0    # connections lost mid-flight
        self.ok = 0

    def _connect(self):
        if self.conn is not None:
            try:
                self.conn.close()
            except Exception:
                pass
        self.conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=30)

    def _once(self, command, args):
        if self.conn is None:
            self._connect()
        body = json.dumps(args)
        self.conn.request(
            "POST", "/call/" + command, body,
            {"Content-Type": "application/json"},
        )
        resp = self.conn.getresponse()
        payload = resp.read()
        return resp.status, payload

    def call(self, command, args):
        """Return the parsed `result`, retrying transient conditions."""
        import time
        for _ in range(RETRY_LIMIT):
            try:
                status, payload = self._once(command, args)
            except Exception:
                self.dropped += 1
                self.conn = None
                time.sleep(RETRY_SLEEP)
                continue
            if status == 503:
                self.deferred += 1
                time.sleep(RETRY_SLEEP)
                continue
            if status != 200:
                raise AssertionError(
                    "worker %d: %s%s -> HTTP %d: %s"
                    % (self.worker_id, command, args, status, payload[:200])
                )
            doc = json.loads(payload)
            if doc.get("status") != "ok":
                raise AssertionError(
                    "worker %d: %s%s -> %s"
                    % (self.worker_id, command, args, payload[:200])
                )
            self.ok += 1
            return doc.get("result")
        raise AssertionError(
            "worker %d: %s%s never completed in %d attempts"
            % (self.worker_id, command, args, RETRY_LIMIT)
        )

    def close(self):
        if self.conn is not None:
            try:
                self.conn.close()
            except Exception:
                pass


EXPECTED_LIST_LEN = 10


def run_worker(port, worker_id, requests, errors, stats):
    client = Client(port, worker_id)
    try:
        for i in range(requests):
            # A value unique to this worker and iteration, so a result that
            # belongs to another request is visible rather than plausible.
            x = worker_id * 1000 + i + 1
            got = client.call("square", [x])
            if got != x * x:
                raise AssertionError(
                    "worker %d: square(%d) returned %r" % (worker_id, x, got)
                )
            # Alternate in the allocation-heavy command so the arena is under
            # pressure for roughly half the run.
            if i % 2 == 0:
                lst = client.call("echoList", [])
                if not isinstance(lst, list) or len(lst) != EXPECTED_LIST_LEN:
                    raise AssertionError(
                        "worker %d: echoList returned %r" % (worker_id, lst)
                    )
    except Exception as exc:  # noqa: BLE001 - reported, not swallowed
        errors.append(str(exc))
    finally:
        stats.append((client.ok, client.deferred, client.dropped))
        client.close()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("port", type=int)
    ap.add_argument("mode", choices=["seq", "conc"])
    ap.add_argument("--requests", type=int, default=200)
    ap.add_argument("--workers", type=int, default=1)
    args = ap.parse_args()

    workers = 1 if args.mode == "seq" else args.workers
    errors = []
    stats = []
    threads = [
        threading.Thread(
            target=run_worker,
            args=(args.port, w, args.requests, errors, stats),
        )
        for w in range(workers)
    ]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

    ok = sum(s[0] for s in stats)
    deferred = sum(s[1] for s in stats)
    dropped = sum(s[2] for s in stats)
    print(
        "mode=%s workers=%d ok=%d deferred=%d dropped=%d errors=%d"
        % (args.mode, workers, ok, deferred, dropped, len(errors))
    )
    for e in errors[:5]:
        print("  " + e, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
