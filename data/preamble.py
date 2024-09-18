import sys
import subprocess
import tempfile
import os
from pymorlocinternals import mlc_serialize, mlc_deserialize

# import only used if dictionaries are passed
from collections import OrderedDict

# imports for ICP
import socket
import time
import multiprocessing
import select
import queue

BUFFER_SIZE = 1024

from contextlib import closing

# These three parameters describe the retru times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30
