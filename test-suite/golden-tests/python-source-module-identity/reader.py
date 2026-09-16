import sys
import plain
import app.main
import app.util
from app.main import state as snapshot_at_import
from app.copy import app_copy_kind

def read_plain(x):
    # `plain` here must be the very module morloc loaded from plain.py, so the
    # value set through set_plain is visible and the two are one object
    same = plain is sys.modules[plain.__name__]
    return "plain:" + str(plain.state) + ":" + plain.__name__ + ":" + str(same)

def read_app(x):
    from app.main import state
    same = app.main is sys.modules["app.main"]
    return "app.main:" + str(state) + ":" + app.main.__name__ + ":" + str(same) + ":" + app.util.tag()

def read_app_copy(x):
    return app_copy_kind(x)
