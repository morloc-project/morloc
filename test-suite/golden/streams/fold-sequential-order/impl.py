import threading


def produce_seq(n, threads, sink):
    # The batches are put one at a time, in order, each by the thread whose
    # turn it is: no two puts are ever in flight together.
    turn = threading.Condition()
    state = {"next": 0, "error": None}

    def run(k):
        while True:
            with turn:
                turn.wait_for(lambda: state["next"] >= n or state["next"] % threads == k)
                i = state["next"]
                if i >= n:
                    return
            try:
                sink([chr(ord("a") + i % 26)])
            except BaseException as e:
                state["error"] = e
            with turn:
                state["next"] = i + 1 if state["error"] is None else n
                turn.notify_all()

    ts = [threading.Thread(target=run, args=(k,)) for k in range(threads)]
    for t in ts:
        t.start()
    for t in ts:
        t.join()
    if state["error"] is not None:
        raise state["error"]
