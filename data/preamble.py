#!/usr/bin/env python

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
from contextlib import closing

PACKET_TYPE_DATA = 0x00
PACKET_TYPE_CALL = 0x01
PACKET_TYPE_GET  = 0x02
PACKET_TYPE_PUT  = 0x03
PACKET_TYPE_PING = 0x04

PACKET_SOURCE_MESG = 0x00 # the message contains the data
PACKET_SOURCE_FILE = 0x01 # the message is a path to a file of data
PACKET_SOURCE_NXDB = 0x02 # the message is a key to the nexus uses to access the data

PACKET_FORMAT_JSON = 0x00

PACKET_COMPRESSION_NONE = 0x00 # uncompressed

PACKET_STATUS_PASS = 0x00
PACKET_STATUS_FAIL = 0x01

PACKET_ENCRYPTION_NONE  = 0x00 # unencrypted

# These three parameters describe the retry times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30

# socket buffer size
BUFFER_SIZE = 4096

def _log(msg, logfile="log"):
    with open(logfile, "a+") as fh:
        print(f"Python3: {msg}", flush=True, file=fh)
