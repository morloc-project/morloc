import os


def here(x):
    return os.environ.get("MOCK_NODE", "parent") + ":" + str(x)
