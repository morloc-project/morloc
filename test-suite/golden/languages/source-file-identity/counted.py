import sys

sys._mlc_identity_test_loads = getattr(sys, "_mlc_identity_test_loads", 0) + 1


def load_count(_):
    return sys._mlc_identity_test_loads
