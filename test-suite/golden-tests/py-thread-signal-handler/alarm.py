import signal
import time


class Timeout(Exception):
    pass


def _on_alarm(sig, frame):
    raise Timeout()


signal.signal(signal.SIGALRM, _on_alarm)


def arm(x):
    signal.alarm(1)
    time.sleep(2)
    return x


def ping(x):
    return x
