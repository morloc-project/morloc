import sys
import tempfile
import os
import json
import struct

# import only used if dictionaries are passed
from collections import OrderedDict

# imports for ICP
import socket
import time
import multiprocessing
import select

BUFFER_SIZE = 4096

from contextlib import closing

# These three parameters describe the retru times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30

def _log(msg, logfile="log"):
    with open(logfile, "a+") as fh:
        print(f"Python3: {msg}", flush=True, file=fh)
