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
import json
import traceback

# AUTO include imports start
# <<<BREAK>>>
# AUTO include imports start

import pympack as mp

# this determines how much whitespace is in the output
JSON_SEPARATORS = (",", ":")

PACKET_TYPE_DATA = 0x00
PACKET_TYPE_CALL = 0x01
PACKET_TYPE_GET  = 0x02
PACKET_TYPE_PUT  = 0x03
PACKET_TYPE_PING = 0x04

PACKET_SOURCE_MESG = 0x00 # the message contains the data
PACKET_SOURCE_FILE = 0x01 # the message is a path to a file of data
PACKET_SOURCE_NXDB = 0x02 # the message is a key to the nexus uses to access the data

PACKET_FORMAT_JSON = 0x00
PACKET_FORMAT_MSGPACK = 0x01

PACKET_COMPRESSION_NONE = 0x00 # uncompressed

PACKET_STATUS_PASS = 0x00
PACKET_STATUS_FAIL = 0x01

PACKET_ENCRYPTION_NONE  = 0x00 # unencrypted

MSGPACK_TYPE_TUPLE = 0

# These three parameters describe the retru times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30

# buffer size for socket IPC
BUFFER_SIZE = 4096

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

def trace(msg):
    return f"Error: {msg}\n\n{traceback.format_exc()}"


def hex(xs: bytes) -> str:
    return ' '.join('{:02x}'.format(x) for x in xs)

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


# JSON deserialization handling

def _deserialize_list(xs, array_schema):
    deserialize = _dispatch_deserialize[array_schema[0]]
    return [deserialize(x, array_schema[1]) for x in xs]


def _deserialize_tuple(xs, params):
    result = []
    for (x, s) in zip(xs, params):
        deserialize = _dispatch_deserialize[s[0]] 
        element = deserialize(x, s[1])
        result.append(element)

    return tuple(result)


def _deserialize_record(d0, params):
    d = dict()
    for [k, v] in params:
        deserializer = _dispatch_deserialize[v[0]]
        d[k] = deserializer(d0[k], v[1])
    return d

_dispatch_deserialize = {
    "a" : _deserialize_list,
    "t" : _deserialize_tuple,
    "m" : _deserialize_record,
    "f" : lambda x, _: x,
    "i" : lambda x, _: x,
    "s" : lambda x, _: x,
    "b" : lambda x, _: x,
    "z" : None
}

def json_deserialize(json_data, schema_str: str, is_file = False):

    schema = mp.parse_schema(schema_str)

    _log(f"Deserializing JSON, is_file={str(is_file)}")
    if is_file:
        with open(json_data, "r") as fh:
            x = json.load(fh)
    else:
        x = json.loads(json_data)

    deserialize = _dispatch_deserialize[schema[0]]

    return deserialize(x, schema[1])




# Socket code

def client(pool, message):

    _log("entering nexus client")

    data = None

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
                fd = s.fileno()
                _log(f"connected to {pool.lang} pool on fd {fd} ...")
                _log(f"sending message on fd {fd}: {message}")
                s.sendall(message)
                _log(f"waiting for response from fd {fd}")

                data = s.recv(BUFFER_SIZE)

                _log(f"data received from {pool.lang} on fd {fd}")
                (_, offset, length) = _read_header(data)

                expected_size = 32 + offset + length
                current_size = len(data) 

                if current_size > expected_size:
                    _log("Too much data from fd {fd}")
                    sys.exit(1)
                elif current_size < expected_size:
                    datas = [data]
                    while current_size < expected_size:
                        data = s.recv(BUFFER_SIZE)
                        datas.append(data)
                        current_size += len(data)
                    data = b"".join(datas)

                _log(f"response received from fd {fd}: {data}")
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

    if data is None:
        _log("failed to retrieve data from client, returning empty string")
        return ""

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
        _log(trace("Unpack failed on format '{fmt}' with exceiption {str(e)}"))
        raise e

    return values

def _pack(fmt, *args):
    fmt = ">" + fmt
    try:
        data = struct.pack(fmt, *args) 
    except Exception as e:
        _log(trace(f"Pack failed on format '{fmt}'"))
        raise e

    return data

def _read_header(data : bytes) -> tuple[bytes, int, int]:
    if len(data) < 32:
        raise ValueError (f"packet is too small '{str(data)}'")

    fields = _unpack("4B4H8sIQ", data[0:32])  

    observed_magic = fields[0:4]
    expected_magic = (0x6d, 0xf8, 0x07,0x07)
    if observed_magic != expected_magic:
        raise ValueError (f"bad magic: observed {observed_magic}, expected {expected_magic}")

    command = fields[8]
    offset = fields[9]
    length = fields[10]

    # return the offset and length
    return (command, offset, length)



def print_return(data, schema_str):
    # Parse the call return packet
    _log("Printing return")
    (cmd, offset, _) = _read_header(data)

    cmd_type = cmd[0]
    status = cmd[5]
    data_start = 32 + offset

    if cmd_type == PACKET_TYPE_DATA and status == PACKET_STATUS_PASS:
        exit_code = 0
        outfile = sys.stdout
    elif cmd_type == PACKET_TYPE_DATA and status == PACKET_STATUS_FAIL:
        exit_code = 1
        outfile = sys.stderr
        error_content = data[data_start: ]
        errmsg = error_content.decode("utf8")
        clean_exit(1, errmsg)
    else:
        clean_exit(1, f"Implementation bug: expected data packet: {str(data)}")

    cmd_source = cmd[1]
    cmd_format = cmd[2]

    isgood = False
    error = ""

    if cmd_source == PACKET_SOURCE_MESG:
        content = data[data_start: ]
    elif cmd_source == PACKET_SOURCE_FILE:
        filename = data[data_start:].decode()
        with open(filename, 'rb') as file:
            content = file.read()
        os.unlink(filename)  # Delete the temporary file


    if cmd_format == PACKET_FORMAT_JSON:
        print(content, file=outfile)
        isgood = True

    if cmd_format == PACKET_FORMAT_MSGPACK:
        try:
            json.dump(mp.unpack(content, schema_str), fp = sys.stdout, separators = JSON_SEPARATORS)
        except Exception as e:
            _log(trace(f"Failed to read output MessagePack format with error {str(e)}:\n hex = {hex(content)}\n chr = {str(content)}"))
        print() # just for that adorable little newline
        isgood = True

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

def _make_data(
    value,
    src    = PACKET_SOURCE_MESG,
    fmt    = PACKET_FORMAT_MSGPACK,
    cmpr   = PACKET_COMPRESSION_NONE,
    encr   = PACKET_ENCRYPTION_NONE,
    status = PACKET_STATUS_PASS,
):
    return _pack(
        "32s{}s".format(len(value)),
        _make_header(
            len(value),
            _pack("BBBBBBxx", PACKET_TYPE_DATA, src, fmt, cmpr, encr, status)
        ),
        value
    )

def _make_ping_packet():
    return _make_header(
      length = 0,
      command = _pack("Bxxxxxxx", PACKET_TYPE_PING)
    )

def get_format_from_extension(file_path):
    extension = os.path.splitext(file_path)[1]
    if extension == ".json":
        return PACKET_FORMAT_JSON
    elif extension in [".mpk", ".msgpack"]:
        return PACKET_FORMAT_MSGPACK
    else:
        print(f"Unexpectd input file extension {extension} in file {file_path}", file=sys.stderr)
        sys.exit(1)

def get_format_from_data(binary_data):
    try:
        # TODO: Just peak at the first 1000 lines or so. This is still not safe,
        # though, since small integers are valid in JSON and MessagePack. "2",
        # for example, would be 50 in MessagePack and 2 in JSON. So there is
        # overlap.
        _ = msgpack_deserialize(binary_data)
        return PACKET_FORMAT_MSGPACK
    except:
        return PACKET_FORMAT_JSON

def handle_json_file_argument(filename, schema):
    """
    Convert the JSON file to MessagePack

    If the file is short, then pass as message.

    Otherwise, write the JSON data to a MessagePack file with extenssion .mpk
    """
    data = mp.pack((json_deserialize(filename, schema, is_file = True)), schema)

    if len(data) <= 65536 - 32:
        return _make_data(data)
    else:
        msgpack_filename = os.path.splitext(filename)[0] + ".mpk"
        with open(msgpack_filename, "wb") as fh:
            fh.write(data)
        return _make_data(msgpack_filename.encode("utf8"), src=PACKET_SOURCE_FILE)

def handle_msgpack_file_argument(filename):
    """
    Read a messagepack file argument
    """
    return _make_data(filename.encode("utf8"), src = PACKET_SOURCE_FILE)

def prepare_call_packet(mid, args, schemas):
    arg_msgs = []
    for (arg, schema) in zip(args, schemas):
        if os.path.isfile(arg):
            fmt = get_format_from_extension(arg)
            if fmt == PACKET_FORMAT_JSON:
                packet = handle_json_file_argument(arg, schema)
            else:
                packet = handle_msgpack_file_argument(arg)
        else:
            is_file = False
            try:
                # if it isn't a file but is readable anyway
                # e.g., the product of file substitution
                with open(arg, "rb") as fh: 
                    data = fh.read().strip()
                is_file = True
            except:
                data = arg.encode("utf8")

            # if data was read from a file, then it might be binary (MessagePack
            # or compressed)
            if is_file:
                if get_format_from_data(data) == PACKET_FORMAT_JSON:
                    packet = _make_data(mp.pack(json_deserialize(data, schema), schema))
                else:
                    packet = _make_data(arg)
            # if data is not from a file, then it must be json
            else:
                packet = _make_data(mp.pack(json_deserialize(data, schema), schema))

        arg_msgs.append(packet)

    call_data_length = sum([len(arg_msg) for arg_msg in arg_msgs])

    call_header = _make_header(
      call_data_length,
      command = _pack("BIxxx", PACKET_TYPE_CALL, mid)
    )

    # find the lengths of all the arguments
    call_format = "32s" + "".join(str(len(n)) + "s" for n in arg_msgs)

    _log(f"Creating call packet with data length of {str(call_data_length)} and format {call_format}")

    return _pack(call_format, call_header, *arg_msgs)
    

def run_command(mid, args, pool_lang, sockets, arg_schema, return_schema):
    result = None
    error = ""

    try:
        # Start language servers
        for (lang, cmd, pipe) in sockets:
            try:
                start_language_server(lang, cmd, pipe)
            except Exception as e:
                _log(trace(f"Failed to start {lang} language server: {str(e)}"))
                raise  # Re-raise the exception to be caught in the outer try block

        message = prepare_call_packet(mid, args, arg_schema)

        # Send arguments over the socket
        result = client(resources["pools"][pool_lang], message)

    except Exception as e:
        error = trace(f"An error occurred in run_command: {str(e)}")
        _log(error)

    # Ensure that processes are stopped and files deleted in all cases
    finally:
        cleanup()

    if error:
        clean_exit(1, error)
    else:
        print_return(result, return_schema)



# AUTO include ui start
# <<<BREAK>>>
# AUTO include ui end



def dispatch(cmd, args):
    if cmd in ["-h", "--help", "-?", "?"]:
        usage()
    else:
        command_table[cmd](args)


if __name__ == "__main__":
    if len(sys.argv) == 1:
        usage()
    else:
        cmd = sys.argv[1]
        args = sys.argv[2:]
        dispatch(cmd, args)
