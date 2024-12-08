#### PREAMBLE

import sys
import tempfile
import os
import struct

# import only used if dictionaries are passed
from collections import OrderedDict

# imports for ICP
import socket
import multiprocessing
import select
from contextlib import closing

# AUTO include imports start
# <<<BREAK>>>
# AUTO include imports end

global_state = {"tmpdir" : ""}

import pympack as mp

#### Packet handling

PACKET_TYPE_DATA = 0x00
PACKET_TYPE_CALL = 0x01
PACKET_TYPE_PING = 0x02
PACKET_TYPE_GET  = 0x03
PACKET_TYPE_POST = 0x04
PACKET_TYPE_PUT  = 0x05
PACKET_TYPE_DEL  = 0x06

PACKET_SOURCE_MESG = 0x00 # the message contains the data
PACKET_SOURCE_FILE = 0x01 # the message is a path to a file of data
PACKET_SOURCE_RPTR = 0x02 # the message is a path to a file of data

PACKET_FORMAT_JSON     = 0x00
PACKET_FORMAT_MSGPACK  = 0x01
PACKET_FORMAT_TEXT     = 0x02
PACKET_FORMAT_DATA     = 0x03
PACKET_FORMAT_VOIDSTAR = 0x04

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

# AUTO include serialization start
# <<<BREAK>>>
# AUTO include serialization end

class FailingPacket(Exception):
    """An exception that passes up a Fail packet"""

    def __init__(self, errmsg, error_code=None):
        _log(f"FailingPacket: {errmsg!s}")
        try:
            self.packet = _make_data(errmsg, status = PACKET_STATUS_FAIL, fmt = PACKET_FORMAT_TEXT)
        except Exception as e:
            errmsg = f"Failed to fail properly: {e!s}"
            _log(errmsg)
            sys.exit(1)
        self.error_code = error_code
        super().__init__(self.packet)

    def __str__(self):
        try:
            errmsg = self.packet[32:]
        except Exception as e:
            return f"Malformed FailingPacket {e!s}"
        if self.error_code:
            return f"FailingPacket ({self.error_code}): {errmsg!s}"
        return f"FailingPacket: {errmsg!s}"

def _unpack(fmt, *args):
    fmt = ">" + fmt
    try:
        values = struct.unpack(fmt, *args) 
    except Exception as e:
        raise FailingPacket(f"Unpack failed on format '{fmt}' with error: {str(e)}")

    return values

def _pack(fmt, *args):
    fmt = ">" + fmt
    try:
        data = struct.pack(fmt, *args) 
    except Exception as e:
        raise FailingPacket(f"Pack failed on format '{fmt}' with error: {str(e)}")

    return data


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
    if isinstance(value, str):
        value = value.encode()
    return _pack(
        "32s{}s".format(len(value)),
        _make_header(
            len(value),
            _pack("BBBBBBxx", PACKET_TYPE_DATA, src, fmt, cmpr, encr, status)
        ),
        value
    )

def _read_header(data : bytes) -> tuple[bytes, int, int]:
    if len(data) < 32:
        _log("packet is too small")
        sys.exit(1)

    fields = _unpack("4B4H8sIQ", data[0:32])  

    # check the morloc magic
    observed_magic = fields[0:4]
    expected_magic = (0x6D, 0xf8, 0x07,0x07)
    if observed_magic != expected_magic:
        _log(f"bad magic: observed {observed_magic}, expected {expected_magic}")

    command = fields[8]
    offset = fields[9]
    length = fields[10]

    # return the offset and length
    return (command, offset, length)

def _get_value(data: bytes, schema_str: str):

    (cmd, offset, _) = _read_header(data)

    cmd_type   = cmd[0]
    cmd_source = cmd[1]
    cmd_format = cmd[2]
    cmd_status = cmd[5]

    if(cmd_type != PACKET_TYPE_DATA):
        raise FailingPacket("Expected a data packet")

    if cmd_format == PACKET_FORMAT_MSGPACK:
        deserializer = lambda x: mp.mesgpack_to_py(x, schema_str)
    elif cmd_format == PACKET_FORMAT_TEXT:
        deserializer = lambda x: x
    elif cmd_format == PACKET_FORMAT_JSON:
        raise FailingPacket("JSON no longer supported inside pools")
    else:
        raise FailingPacket(f"Invalid format {cmd_format!s}")

    data_start = 32 + offset

    result = None

    if cmd_source == PACKET_SOURCE_MESG:
        try:
            result = (deserializer(data[data_start:]))
        except Exception as e:
            raise FailingPacket(f"Failed to parse msg packet: {e!s}")
    elif cmd_source == PACKET_SOURCE_FILE:
        try:
            filename = data[data_start:].decode()
            with open(filename, 'rb') as file:
                result = deserializer(file.read())
        except Exception as e:
            raise FailingPacket(f"Failed to parse file packet: {e!s}")
    else:
        raise FailingPacket("Invalid source" )

    if(cmd_status == PACKET_STATUS_FAIL):
        raise FailingPacket(result)
    else:
        return result


def _put_value(value, schema_str: str) -> bytes:
    """
    Takes an encoded value and returns a packet representing it. This packet may
    be read by another _get_value function to retrieve the encoded value.

    If the value is small, the value may be stored in the packet itself. If it
    is larger, then it may be stored in a database or file and the packet will
    be a key or filename needed to retrieve it.
    """

    _log("1 .....")

    try:
        _log(f"value = {value!s}")
        _log(f"schema = {schema_str!s}")
        data = mp.py_to_mesgpack(value, schema_str)
        _log("1a")
    except Exception as e:
        _log("1b")
        raise FailingPacket(f"Could not serialize data: {e!s}")

    _log("2")

    if len(data) <= 65536 - 32:
        return _make_data(data)
    else:
        # for large data, write a temporary file
        with tempfile.NamedTemporaryFile(delete=False, dir=global_state["tmpdir"], mode='wb') as temp_file:
            temp_file.write(data)
            tmpfilename = temp_file.name
        return _make_data(tmpfilename.encode("utf8"), src=PACKET_SOURCE_FILE)

def _stream_data(conn):
    first_packet = conn.recv(BUFFER_SIZE)
    try:
        (_, msg_offset, msg_length) = _read_header(first_packet) 
        packet_size = 32 + msg_offset + msg_length
    except Exception as e:
        raise FailingPacket(f"Could not process header: {e!s}")

    if(len(first_packet) == packet_size):
        return first_packet
    elif(len(first_packet) > packet_size):
        raise FailingPacket(f"Packet is longer than expected")
    else:
        packets = [first_packet]
        total_length = len(first_packet)

        while(total_length < packet_size):
            new_packet = conn.recv(BUFFER_SIZE)
            packets.append(new_packet)
            total_length += len(new_packet)

        return b''.join(packets)

def _request_from_socket(socket_path, message):
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        try:
            _log(f"Connecting to {socket_path!s}")
            s.connect(socket_path)
            fd = s.fileno()
            _log(f"Connected to {socket_path} on file descriptor {fd!s}")
            
            _log(f"Sending message '{message}' to {socket_path} on fd {fd!s}")
            s.send(message)
            
            _log(f"Requesting data on fd {fd!s}")
            data = _stream_data(s)
            _log(f"Data {data!s} received from {socket_path!s} on fd {fd!s}")
        except Exception as e:
            raise FailingPacket(f"Failed socket connection: {e!s}")
    return data


def _morloc_foreign_call(pool_pipe, manifold_id, args):
    _log("Making foreign call")
    _log(f"pool_pipe={pool_pipe!s}")
    _log(f"manifold_id={manifold_id!s}")
    _log(f"args={args!s}")

    for arg in args:
        _log(f"arg header = {arg[0:32]!s}")
        _log(f"arg content = {arg[32:]!s}")

    call_data_length = sum([len(arg) for arg in args])

    _log(f"call_data_length = {call_data_length!s}") 
    
    call_header = _make_header(
      call_data_length,
      command = _pack("BIxxx", PACKET_TYPE_CALL, manifold_id)
    )
    
    # find the lengths of all the arguments
    call_format = "32s" + "".join(str(len(arg)) + "s" for arg in args)
    msg = _pack(call_format, call_header, *args)
    
    _log(f"Creating call packet with data length of {call_data_length!s} and format {call_format!s}")

    # This should be a data object
    return _request_from_socket(pool_pipe, msg)


# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end

# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end



#### MAIN

def message_response(data):
    (msg_cmd, msg_offset, msg_length) = _read_header(data) 

    _log(f"msg_cmd = {msg_cmd!s}")

    data_start = 32 + msg_offset

    msg_cmd_type = msg_cmd[0]

    if(msg_cmd_type == PACKET_TYPE_PING):
        result = _make_header(
          length = 0,
          command = _pack("Bxxxxxxx", PACKET_TYPE_PING)
        )
    elif(msg_cmd_type == PACKET_TYPE_CALL):

        cmdID = _unpack("I", msg_cmd[1:5])[0]

        args = []
        while(data_start < msg_length):
            _log(f"Parsing arg header from index {data_start!s}")
            (_, arg_offset, arg_length) = _read_header(data[data_start:data_start+32])
            _log(f"Parsing arg with offset {arg_offset!s} and length {arg_length!s}")
            args.append(data[data_start:(data_start + 32 + arg_offset + arg_length)])
            data_start += 32 + arg_offset + arg_length

        _log(f"dispatching on {cmdID!s}")

        if cmdID not in dispatch:
            raise FailingPacket(f"Internal error in python pool: no manifold found with id={cmdID!s}")

        mlc_function = dispatch[cmdID]

        try:
            result = mlc_function(*args)

        except FailingPacket as e:
            raise FailingPacket(f"Forwarding fail from m{cmdID!s}: {e!s}")

        except Exception as e:
            raise FailingPacket(f"Error in m{cmdID!s}: {e!s}")

        _log(f"from cmdID {cmdID!s} pool returning message of length '{result!s}'")
    else:
        raise FailingPacket(f"Expected a call packet, found {msg_cmd_type!s}")

    return result


def worker(data, result_queue):
    try:
        _log(f"Worker started")
        result = message_response(data)
        _log(f"Worker ended with result '{result!s}'")
        _log("Worker putting result in queue")
        result_queue.put(
            result, block=True, timeout=None
        )  # block until a free spot is available in the queue
        _log("Worker put result in queue")
        _log(f"New queue size: {result_queue.qsize()!s}")
    except Exception as e:
        _log(f"Worker error: {e!s}")
        result_queue.put(
            None, block=True, timeout=None
        )  # Put None to indicate an error
    finally:
        _log("Worker function exiting")


def server(socket_path):
    _log("\n---------------------")
    _log("Enter server function")

    if os.path.exists(socket_path):
        os.unlink(socket_path)

    queue = []

    with closing(socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)) as s:
        s.bind(socket_path)
        s.listen(1)
        s.setblocking(False)
        _log(f"Server listening on {socket_path!s}")

        while True:

            ready, _, _ = select.select([s], [], [], 0.0001)

            if ready:
                conn, _ = s.accept()
                _log(f"Connected on fd {conn.fileno()!s}")
                data = _stream_data(conn)

                if data:
                    _log(f"Job starting on fd {conn.fileno()!s}")
                    result_queue = multiprocessing.Queue()
                    p = multiprocessing.Process(
                        target=worker, args=(data, result_queue)
                    )
                    queue.append((p, conn, result_queue))
                    p.start()

            for job in queue[:]:
                p, conn, result_queue = job

                if not result_queue.empty():
                    try:
                        _log(f"Processing queue result for fd {conn.fileno()!s}")
                        result = result_queue.get(block=True)
                        _log(f"got result on fd {conn.fileno()!s}")

                        if result is not None:
                            _log(f"Sending result on fd {conn.fileno()!s}")
                            conn.send(result)
                            _log(f"Sent result for process {p.pid!s} on fd {conn.fileno()!s}")
                    except Exception as e:
                        _log(f"failed to get result from queue: {e!s} on fd {conn.fileno()!s}")
                elif not p.is_alive():
                    # Send an empty message signaling failure
                    errmsg = f"Process {p.pid!s} on fd {conn.fileno()!s} not alive and no result available"
                    error_packet = _make_data(errmsg, status = PACKET_STATUS_FAIL, fmt = PACKET_FORMAT_TEXT)
                    conn.send(error_packet)
                else:
                    # the process is still alive and no data has been reveived
                    # so we continue to wait
                    continue

                try:
                    # Process is done or we got a result, clean up
                    _log(f"Closing connection for process {p.pid!s} on fd {conn.fileno()!s}")
                    conn.close()
                    _log(f"Removing job for process {p.pid!s} on fd {conn.fileno()!s}")
                    queue.remove(job)
                    _log(f"Joining process {p.pid!s} on fd {conn.fileno()!s}")
                    p.join(timeout=0.0001)  # Wait for up to 1ms
                    if p.is_alive():
                        _log(f"Force terminating process {p.pid!s} on fd {conn.fileno()!s}")
                        p.terminate()
                        p.join(timeout=0.0001)  # Wait again to ensure termination
                    _log(f"Finished handling process {p.pid!s} on fd {conn.fileno()!s}")
                except Exception as e:
                    _log(f"failed to cleanup properly: {e!s} on fd {conn.fileno()!s}")


if __name__ == "__main__":
    try:
        socket_path = sys.argv[1]
        global_state["tmpdir"] = sys.argv[2]
        server(socket_path)
    except Exception as e:
        _log(f"Python pool failed: {e!s}")
        sys.exit(1)

