import time


def hang(marker):
    with open(marker, "w") as fh:
        fh.write("started\n")
    time.sleep(60)
