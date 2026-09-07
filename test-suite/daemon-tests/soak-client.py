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
  mode `seq`   -- one connection, --requests calls in series
  mode `conc`  -- --workers connections in parallel, --requests calls each
  mode `churn` -- as `conc`, but every call carries a list long enough to be
                  handed over in shared memory rather than inside the packet,
                  so the allocate / hand across / free / reuse path is the one
                  under pressure

Workers wait on a barrier and start together, and `--rounds` repeats that
burst. A race between two dispatches needs them to overlap; threads that
drift apart as they start stop overlapping, and a burst that happens once
only samples the schedule once.

Prints one summary line of `key=value` pairs and exits non-zero if any
request failed to complete or returned the wrong value.
"""
import argparse
import http.client
import json
import sys
import threading
import time

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


def explain_square(x, got):
    """Say whether a wrong square is another request's answer.

    Arguments are `(worker + 1) * 100000 + round * 1000 + i + 1`. If the
    value returned is the exact square of a DIFFERENT well-formed argument,
    the pool computed the right function on the wrong input -- which is a
    different fault from returning damaged memory, and worth telling apart
    without another CI round trip."""
    if not isinstance(got, (int, float)):
        return " (not a number)"
    if isinstance(got, float) and (got != got or got in (float("inf"), float("-inf"))):
        return " (not finite -- uninitialised memory)"
    if got == 0:
        return " (zero -- a zeroed block reads as 0)"
    try:
        root = round(got ** 0.5)
    except (OverflowError, ValueError):
        return " (unrepresentable)"
    if root * root == got and root != x:
        worker = root // 100000 - 1
        rest = root % 100000
        if 0 <= worker < 64:
            return (" == %d^2, i.e. the square of worker %d's argument "
                    "(round %d, call %d) -- right function, wrong input"
                    % (root, worker, rest // 1000, rest % 1000))
        return " == %d^2, the square of some other argument" % root
    return " (not the square of any well-formed argument -- damaged memory)"


def payload_for(worker_id, iteration, size):
    """A list whose every element identifies the request that asked for it.

    Length alone would catch a truncated reply and nothing else. Contents
    keyed to the worker and iteration mean a payload belonging to another
    request in flight fails on the first element rather than looking
    plausible."""
    base = (worker_id + 1) * 1000000 + iteration * 1000
    return [base + i for i in range(size)]


def whose_payload(value, index):
    """Name the request a stray payload element belongs to, or None.

    Elements are `base + index` with the base encoding worker and iteration
    (see payload_for), so a value that arrived in the wrong reply still says
    where it came from. That distinguishes a block recycled into another
    live request from a block filled with something else entirely."""
    base = value - index
    if base <= 0 or base % 1000 != 0:
        return None
    worker = base // 1000000 - 1
    iteration = (base % 1000000) // 1000
    if 0 <= worker < 64 and 0 <= iteration < 100000:
        return "worker %d iteration %d" % (worker, iteration)
    return None


def describe_corruption(sent, got):
    """Say what SHAPE the damage has, not merely that there is damage.

    The three cases tell different stories and want different fixes. A
    correct prefix followed by zeros is a block zeroed underneath the reader
    while it read -- shfree zero-fills on the final reference drop, so that
    is a use-after-free. A reply that is wholly another request's payload is
    a block already recycled before the read started. Anything else is
    neither, and worth seeing in full."""
    n = len(sent)
    prefix = 0
    while prefix < n and prefix < len(got) and sent[prefix] == got[prefix]:
        prefix += 1
    tail = got[prefix:]
    zeros = sum(1 for v in tail if v == 0)
    nonzero = [(prefix + k, v) for k, v in enumerate(tail) if v != 0][:3]
    head = "%d/%d elements correct" % (prefix, n)
    if tail and zeros == len(tail):
        return head + ", then %d zeros to the end (block zeroed under the reader)" % zeros
    bits = ["%s, then %d of %d remaining are zero" % (head, zeros, len(tail))]
    for idx, v in nonzero:
        owner = whose_payload(v, idx)
        bits.append(
            "element %d = %d%s"
            % (idx, v, (" -- belongs to %s" % owner) if owner else "")
        )
    return "; ".join(bits)


def check_echo(worker_id, command, sent, got):
    if not isinstance(got, list):
        raise AssertionError(
            "worker %d: %s returned %r, not a list" % (worker_id, command, got)
        )
    if len(got) != len(sent):
        raise AssertionError(
            "worker %d: %s returned %d elements, sent %d (a zeroed length "
            "field reads as an empty list)"
            % (worker_id, command, len(got), len(sent))
        )
    if got != sent:
        raise AssertionError(
            "worker %d: %s corrupted -- %s"
            % (worker_id, command, describe_corruption(sent, got))
        )


def run_worker(port, worker_id, requests, errors, stats, barrier, rounds,
               churn, payload):
    client = Client(port, worker_id)
    try:
        for rnd in range(rounds):
            # Start together. Workers that trickle in serialise themselves,
            # which is the opposite of what this is for.
            if barrier is not None:
                barrier.wait(timeout=120)
            for i in range(requests):
                # A value unique to this worker and iteration, so a result
                # that belongs to another request is visible rather than
                # plausible.
                x = (worker_id + 1) * 100000 + rnd * 1000 + i + 1
                started = time.monotonic()
                got = client.call("square", [x])
                elapsed = time.monotonic() - started
                if got != x * x:
                    # Retry the identical request at once. A correct answer
                    # second time says the damage was to memory in flight
                    # rather than to anything durable, which is most of the
                    # question.
                    retry = client.call("square", [x])
                    raise AssertionError(
                        "worker %d: square(%d) returned %r%s; %s; "
                        "call took %.3fs; immediate retry %s"
                        % (worker_id, x, got, explain_square(x, got),
                           "retry CORRECT" if retry == x * x
                           else "retry ALSO WRONG (%r)" % (retry,),
                           elapsed,
                           "ok" if retry == x * x else "bad")
                    )
                if churn:
                    # Both languages, alternating, so a block recycled under
                    # one pool's threading is not mistaken for evidence about
                    # the other.
                    command = "cppEcho" if i % 2 == 0 else "pyEcho"
                    sent = payload_for(worker_id, rnd * requests + i, payload)
                    check_echo(worker_id, command,
                               sent, client.call(command, [sent]))
                elif i % 2 == 0:
                    # Allocation-heavy, so the arena is under pressure for
                    # roughly half the run.
                    lst = client.call("echoList", [])
                    if not isinstance(lst, list) or len(lst) != EXPECTED_LIST_LEN:
                        raise AssertionError(
                            "worker %d: echoList returned %r" % (worker_id, lst)
                        )
    except Exception as exc:  # noqa: BLE001 - reported, not swallowed
        errors.append(str(exc))
        if barrier is not None:
            # A worker that stops early must not strand the others on the
            # next round's barrier.
            barrier.abort()
    finally:
        stats.append((client.ok, client.deferred, client.dropped))
        client.close()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("port", type=int)
    ap.add_argument("mode", choices=["seq", "conc", "churn"])
    ap.add_argument("--requests", type=int, default=200)
    ap.add_argument("--workers", type=int, default=1)
    ap.add_argument("--rounds", type=int, default=1)
    # Past the size at which a value stops riding inside the packet: eight
    # bytes an element against a 64 KB threshold, so twelve thousand is
    # comfortably over whichever way the element type is counted.
    ap.add_argument("--payload", type=int, default=12000)
    args = ap.parse_args()

    workers = 1 if args.mode == "seq" else args.workers
    errors = []
    stats = []
    barrier = threading.Barrier(workers) if workers > 1 else None
    threads = [
        threading.Thread(
            target=run_worker,
            args=(args.port, w, args.requests, errors, stats, barrier,
                  args.rounds, args.mode == "churn", args.payload),
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
        "mode=%s workers=%d rounds=%d ok=%d deferred=%d dropped=%d errors=%d"
        % (args.mode, workers, args.rounds, ok, deferred, dropped, len(errors))
    )
    # Every failure, not a sample: the shape of the damage varies between
    # them and the variation is the evidence.
    for e in errors:
        print("  " + e, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
