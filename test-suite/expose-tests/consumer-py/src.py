# Imports the exposed Python subpackage. Resolution depends on
# $MORLOC_HOME/lib/python on sys.path (set by the py preamble) and on
# the install pipeline having mapped `morloc-test-expose-py` to the
# Python-legal name `morloc_test_expose_py`.
from morloc_test_expose_py.pkg.sub.util import util_add_one, doubler


def compute(x):
    return doubler(util_add_one(x))
