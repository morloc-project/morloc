#!/usr/bin/env python3

import socket
import subprocess
import sys
import os
import signal
import time
import threading
import queue
import struct

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


# These three parameters describe the retru times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30

# buffer size for socket IPC
BUFFER_SIZE = 1024

# default pipe names, must match the signal naming conventions of the pools
PYTHON_PIPE = "python3-pipe"
CPP_PIPE = "cpp-pipe"
R_PIPE = "r-pipe"

# Resources that need to be cleaned up when the session ends
resources = {"pools": {}, "files": []}


class Pool:
    def __init__(self, lang, process, pipe, stderr_queue, exit_status_queue):
        self.lang = lang
        self.process = process
        self.pipe = pipe
        self.stderr_queue = stderr_queue
        self.exit_status_queue = exit_status_queue


def _log(msg):
    with open("log", "a+") as fh:
        print(f"Nexus: {msg}", file=fh)


def cleanup():
    pools = resources["pools"]
    files = resources["files"]

    _log(f"Cleaning up")
    for pool in pools.values():
        if pool.process.poll() is None:  # If process is still running
            pool.process.terminate()  # Send SIGTERM
            try:
                pool.process.wait(timeout=5)  # Wait for up to 5 seconds
            except subprocess.TimeoutExpired:
                pool.process.kill()  # If still running, send SIGKILL

        # remove the socket pipe file
        try:
            os.unlink(pool.pipe)
        except:
            # this socket file may have been cleaned up by the pool
            pass

    # clean up temporary files and sockets
    for is_temp, file in files:
        if is_temp and os.path.exists(file):
            os.unlink(file)


def clean_exit(exit_code, msg=""):
    cleanup()
    if msg:
        print(msg, file=sys.stderr)
    sys.exit(exit_code)


def signal_handler(sig, frame):
    clean_exit(1)


# Register signal handler for ctrl-c
# This avoids dumping a trace when a user kills a job
signal.signal(signal.SIGINT, signal_handler)


def client(pool, message):

    _log("entering nexus client")

    data = b""

    # it may take awhile for the pool to initialize and create the socket
    delay_time = INITIAL_RETRY_DELAY
    _log(f"contacting {pool.lang} pool ...")
    for attempt in range(MAX_RETRIES):
        # check to see if the pool has died
        if not pool.exit_status_queue.empty():
            exit_status = pool.exit_status_queue.get()
            print(
                f"{pool.lang} pool ended early with exit status {exit_status} and the error message:",
                file=sys.stderr,
            )
            while not pool.stderr_queue.empty():
                print(pool.stderr_queue.get(), file=sys.stderr, end="")
            clean_exit(1, "")
        # try to connect to the pool server
        try:
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
                s.connect(pool.pipe)
                _log(f"connected to {pool.lang} pool ...")
                _log(f"sending message: {message}")
                s.sendall(message)
                _log(f"waiting for response")
                data = s.recv(BUFFER_SIZE)
                _log(f"response received: {data}")
            break
        # try try again
        except (FileNotFoundError, ConnectionRefusedError) as e:
            if attempt == MAX_RETRIES - 1:
                _log(f"Timeout while waiting for {pool.lang} pool")
                raise e
            else:
                time.sleep(delay_time)
                delay_time *= RETRY_MULTIPLIER

    _log("exiting nexus client")

    return data


def start_language_server(lang, cmd, pipe):
    # Create a queue to store stderr output
    stderr_queue = queue.Queue()

    # Create a queue to store the exit status
    exit_status_queue = queue.Queue()

    # Function to read stderr and put it in the queue
    def read_stderr(process, queue):
        for line in process.stderr:
            queue.put(line)
        process.stderr.close()

    # Function to monitor process and get exit status
    def monitor_process(process, queue):
        exit_status = process.wait()
        queue.put(exit_status)

    # Start the language server in the background
    _log(f"Starting server with {cmd} ...")
    process = subprocess.Popen(
        cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True
    )
    _log(f"Server started")

    # Start thread to read stderr
    stderr_thread = threading.Thread(target=read_stderr, args=(process, stderr_queue))
    stderr_thread.daemon = True
    stderr_thread.start()

    # Start thread to monitor process and get exit status
    monitor_thread = threading.Thread(
        target=monitor_process, args=(process, exit_status_queue)
    )
    monitor_thread.daemon = True
    monitor_thread.start()

    pool = Pool(lang, process, pipe, stderr_queue, exit_status_queue)

    resources["pools"][lang] = pool

    # ping the server, make sure it is up and going
    _log(f"Pinging the {lang} server ...")
    pong = client(pool, _make_ping_packet())
    _log(f"Pong from {lang} - server is good (len(pong) == {len(pong)})")

    return pool

def _unpack(fmt, *args):
    fmt = ">" + fmt
    try:
        values = struct.unpack(fmt, *args) 
    except Exception as e:
        _log(f"Unpack failed on format '{fmt}'")
        raise e

    return values

def _pack(fmt, *args):
    fmt = ">" + fmt
    try:
        data = struct.pack(fmt, *args) 
    except Exception as e:
        _log(f"Pack failed on format '{fmt}'")
        raise e

    return data

def _read_header(data : bytes) -> tuple[bytes, int, int]:
    if len(data) < 32:
        _log(f"packet is too small '{str(data)}'")
        sys.exit(1)

    fields = _unpack("4B4H8sIQ", data[0:32])  

    observed_magic = fields[0:4]
    expected_magic = (0x6D, 0xf8, 0x07,0x07)
    if observed_magic != expected_magic:
        _log(f"bad magic: observed {observed_magic}, expected {expected_magic}")

    if len(data) != 32 + fields[10]:
        _log("packet is of unexpected length")
        sys.exit(1)

    command = fields[8]
    offset = fields[9]
    length = fields[10]

    # return the offset and length
    return (command, offset, length)

def print_return(data):
    # Parse the call return packet
    (cmd, offset, _) = _read_header(data)

    start = 32 + offset

    cmd_type = cmd[0]
    status = cmd[5]

    if cmd_type == PACKET_TYPE_DATA and status == PACKET_STATUS_PASS:
        exit_code = 0
        outfile = sys.stdout
    elif cmd_type == PACKET_TYPE_DATA and status == PACKET_STATUS_FAIL:
        exit_code = 1
        outfile = sys.stderr
    else:
        clean_exit(1, f"Implementation bug: expected data packet: {str(data)}")

    data_start = 32 + offset

    cmd_source = cmd[1]
    cmd_format = cmd[2]

    isgood = False
    error = ""

    if cmd_source == PACKET_SOURCE_MESG:
        if cmd_format == PACKET_FORMAT_JSON:
            print(data[data_start:].decode(), file=outfile)
            isgood = True
        else:
            _log("Invalid format")
    elif cmd_source == PACKET_SOURCE_FILE:
        if cmd_format == PACKET_FORMAT_JSON:
            filename = data[data_start:].decode()
            with open(filename, 'r') as file:
                content = file.read()
            os.unlink(filename)  # Delete the temporary file
            print(content, file=outfile)
            isgood = True
        else:
            error = "Invalid format"
    else:
        error = "Not yet supported"

    # Exit with the proper error code
    if isgood:
        clean_exit(exit_code)
    else:
        clean_exit(1, error)


def _make_header(
    length: int,
    command: bytes,
    plain: int = 0,
    version: int = 0,
    version_flavor: int = 0,
    mode: int = 0,
    offset: int = 0
) -> bytes:
    if len(command) != 8:
        _log("Bad command")
    return _pack(
        "4B4H8sIQ",
        0x6D, # m
        0xf8, # o
        0x07, # ding
        0x07, # ding
        plain,
        version,
        version_flavor,
        mode,
        command,
        offset,
        length
    )

def _make_ping_packet():
    return _make_header(
      length = 0,
      command = _pack("Bxxxxxxx", PACKET_TYPE_PING)
    )

def prepare_call_packet(mid, args):
    arg_msgs = []
    for arg in args:
        arg = arg.encode("utf8")
        if os.path.isfile(arg):
            header = _make_header(
              length = len(arg),
              command = _pack(
                "BBBBBxxx",
                PACKET_TYPE_DATA,
                PACKET_SOURCE_FILE,
                PACKET_FORMAT_JSON,
                PACKET_COMPRESSION_NONE,
                PACKET_ENCRYPTION_NONE
              )
            )
        else:
            try:
                # if it isn't a file but is readable anyway
                # e.g., the product of file substitution
                with open(arg, "rb") as fh: 
                    arg = fh.read().strip()
            except:
                pass
            header = _make_header(
              length = len(arg),
              command = _pack(
                "BBBBBxxx",
                PACKET_TYPE_DATA,
                PACKET_SOURCE_MESG,
                PACKET_FORMAT_JSON,
                PACKET_COMPRESSION_NONE,
                PACKET_ENCRYPTION_NONE
              )
            )
        arg_msgs.append(_pack("32s{}s".format(len(arg)), header, arg))

    call_data_length = sum([len(arg_msg) for arg_msg in arg_msgs])

    call_header = _make_header(
      call_data_length,
      command = _pack("BIxxx", PACKET_TYPE_CALL, mid)
    )

    # find the lengths of all the arguments
    call_format = "32s" + "".join(str(len(n)) + "s" for n in arg_msgs)

    _log(f"Creating call packet with data length of {str(call_data_length)} and format {call_format}")

    return _pack(call_format, call_header, *arg_msgs)
    

def run_command(mid, args, pool_lang, sockets):
    result = None
    error = ""

    try:
        # Start language servers
        for (lang, cmd, pipe) in sockets:
            try:
                start_language_server(lang, cmd, pipe)
            except Exception as e:
                _log(f"Failed to start {lang} language server: {str(e)}")
                raise  # Re-raise the exception to be caught in the outer try block

        message = prepare_call_packet(mid, args)

        # Send arguments over the socket
        result = client(resources["pools"][pool_lang], message)

    except Exception as e:
        _log(f"An error occurred: {str(e)}")
        # You might want to set a specific error value or re-raise the exception
        # depending on how you want to handle errors at a higher level
        error = str(e)

    # Ensure that processes are stopped and files deleted in all cases
    finally:
        cleanup()

    if error:
        clean_exit(1, error)
    else:
        print_return(result)
