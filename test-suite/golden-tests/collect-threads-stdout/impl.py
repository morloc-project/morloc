import threading


def produce_threads(sink):
    def run(t):
        for b in range(5):
            sink([t * 100 + k for k in range(b + 1)])

    ts = [threading.Thread(target=run, args=(t,)) for t in range(4)]
    for t in ts:
        t.start()
    for t in ts:
        t.join()
