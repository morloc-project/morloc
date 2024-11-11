#### PREAMBLE

import sys
import os
import tempfile
import struct
import mmap

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
PACKET_TYPE_GET  = 0x02
PACKET_TYPE_PUT  = 0x03
PACKET_TYPE_PING = 0x04

PACKET_SOURCE_MESG = 0x00 # the message contains the data
PACKET_SOURCE_FILE = 0x01 # the message is a path to a file of data
PACKET_SOURCE_MMAP = 0x02 # the message is a memory mapped file

PACKET_FORMAT_JSON = 0x00
PACKET_FORMAT_MSGPACK = 0x01

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

class FailingPacket(Exception):
    """An exception that passes up a Fail packet"""

    def __init__(self, errmsg, error_code=None):
        try:
            errmsg = mp.pack(errmsg.encode("utf8"), "s")
        except:
            pass
        try:
            self.packet = _make_data(errmsg, status = PACKET_STATUS_FAIL)
        except Exception as e:
            errmsg = f"Failed to fail properly: {str(e)}"
            _log(errmsg)
            sys.exit(1)
        self.error_code = error_code
        super().__init__(self.packet)

    def __str__(self):
        try:
            errmsg = mp.unpack(self.packet[32:], "s")
        except Exception as e:
            return f"Malformed FailingPacket {str(e)}"
        if self.error_code:
            return f"FailingPacket ({self.error_code}): {errmsg}"
        return f"FailingPacket: {errmsg}"


def _get_value(data: bytes, schema_str: str):

    (cmd, offset, _) = _read_header(data)

    cmd_type   = cmd[0]
    cmd_source = cmd[1]
    cmd_format = cmd[2]

    if(cmd_type != PACKET_TYPE_DATA):
        raise FailingPacket("Expected a data packet")

    if cmd_format == PACKET_FORMAT_MSGPACK:
        deserializer = lambda x: mp.unpack(x, schema_str)
    elif cmd_format == PACKET_FORMAT_JSON:
        raise FailingPacket("JSON no longer supported inside pools")
    else:
        raise FailingPacket(f"Invalid format {str(cmd_format)}")

    data_start = 32 + offset

    if cmd_source == PACKET_SOURCE_MESG:
        try:
            return(deserializer(data[data_start:]))
        except Exception as e:
            raise FailingPacket(f"Failed to parse msg packet: {str(e)}")
    elif cmd_source == PACKET_SOURCE_MMAP:
        try:
            filename = data[data_start:].decode()
            with open(filename, "rb") as fh:
                mm = mmap.mmap(fh.fileno(), length=0, flags=mmap.MAP_PRIVATE, prot=mmap.PROT_READ)
                obj = deserializer(mm[:])
                mm.close()
                return(obj)
        except Exception as e:
            raise FailingPacket(f"Failed to parse file packet: {str(e)}")
    elif cmd_source == PACKET_SOURCE_FILE:
        try:
            filename = data[data_start:].decode()
            with open(filename, 'rb') as file:
                return(deserializer(file.read()))
        except Exception as e:
            raise FailingPacket(f"Failed to parse file packet: {str(e)}")
    else:
        raise FailingPacket("Invalid source" )

def _put_value(value, schema_str: str) -> bytes:
    """
    Takes an encoded value and returns a packet representing it. This packet may
    be read by another _get_value function to retrieve the encoded value.

    If the value is small, the value may be stored in the packet itself. If it
    is larger, then it may be stored in a database or file and the packet will
    be a key or filename needed to retrieve it.
    """

    try:
        data = mp.pack(value, schema_str)
    except Exception as e:
        raise FailingPacket(f"Could not serialize data: {str(e)}")

    if len(data) <= 65536 - 32:
        return _make_data(data)
    else:
        # for large data, write a temporary file
        with tempfile.NamedTemporaryFile(delete=False, dir=global_state["tmpdir"], mode='wb') as temp_file:
            temp_file.write(data)
            tmpfilename = temp_file.name

        # Create the file without writing any data
        with open(tmpfilename, "wb") as fh:
            fh.truncate(len(data))  # Set the file size without writing zeros
        
        # Memory-map the file
        with open(tmpfilename, "r+b") as fh:
            mm = mmap.mmap(fh.fileno(), len(data), access=mmap.ACCESS_WRITE)
            mm.write(data)

        return _make_data(tmpfilename.encode("utf8"), src=PACKET_SOURCE_MMAP)


def _stream_data(conn):
    first_packet = conn.recv(BUFFER_SIZE)
    try:
        (_, msg_offset, msg_length) = _read_header(first_packet) 
        packet_size = 32 + msg_offset + msg_length
    except Exception as e:
        raise FailingPacket(f"Could not process header: {str(e)}")

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
            _log(f"Connecting to {socket_path}")
            s.connect(socket_path)
            fd = s.fileno()
            _log(f"Connected to {socket_path} on file descriptor {fd}")
            
            _log(f"Sending message '{message}' to {socket_path} on fd {fd}")
            s.send(message)
            
            _log(f"Requesting data on fd {fd}")
            data = _stream_data(s)
            _log(f"Data {data} received from {socket_path} on fd {fd}")
        except Exception as e:
            raise FailingPacket(f"Failed socket connection: {str(e)}")
    return data


def _morloc_foreign_call(pool_pipe, manifold_id, args):
    _log("Making foreign call")
    _log(f"pool_pipe={pool_pipe}")
    _log(f"manifold_id={str(manifold_id)}")
    _log(f"args={str(args)}")

    for arg in args:
        _log(f"arg header = {str(arg[0:32])}")
        _log(f"arg content = {str(arg[32:])}")

    call_data_length = sum([len(arg) for arg in args])

    _log(f"call_data_length = {str(call_data_length)}") 
    
    call_header = _make_header(
      call_data_length,
      command = _pack("BIxxx", PACKET_TYPE_CALL, manifold_id)
    )
    
    # find the lengths of all the arguments
    call_format = "32s" + "".join(str(len(arg)) + "s" for arg in args)
    msg = _pack(call_format, call_header, *args)
    
    _log(f"Creating call packet with data length of {str(call_data_length)} and format {call_format}")

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

    _log(f"msg_cmd = {str(msg_cmd)}")

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
            _log(f"Parsing arg header from index {data_start}")
            (_, arg_offset, arg_length) = _read_header(data[data_start:data_start+32])
            _log(f"Parsing arg with offset {str(arg_offset)} and length {str(arg_length)}")
            args.append(data[data_start:(data_start + 32 + arg_offset + arg_length)])
            data_start += 32 + arg_offset + arg_length

        _log(f"dispatching on {str(cmdID)}")

        if cmdID not in dispatch:
            raise FailingPacket(f"Internal error in python pool: no manifold found with id={str(cmdID)}")

        mlc_function = dispatch[cmdID]

        try:
            result = mlc_function(*args)

        except FailingPacket as e:
            raise FailingPacket(f"Forwarding fail from m{str(cmdID)}: {str(e)}")

        except Exception as e:
            raise FailingPacket(f"Error in m{str(cmdID)}: {str(e)}")

        _log(f"from cmdID {str(cmdID)} pool returning message of length '{len(result)}'")
    else:
        raise FailingPacket(f"Expected a call packet, found {str(msg_cmd_type)}")

    return result


def worker(data, result_queue):
    try:
        _log(f"Worker started")
        result = message_response(data)
        _log(f"Worker ended with result '{result}'")
        _log("Worker putting result in queue")
        result_queue.put(
            result, block=True, timeout=None
        )  # block until a free spot is available in the queue
        _log("Worker put result in queue")
        _log(f"New queue size: {str(result_queue.qsize())}")
    except Exception as e:
        _log(f"Worker error: {str(e)}")
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
        _log(f"Server listening on {socket_path}")

        while True:

            ready, _, _ = select.select([s], [], [], 0.0001)

            if ready:
                conn, _ = s.accept()
                _log(f"Connected on fd {conn.fileno()}")
                data = _stream_data(conn)

                if data:
                    _log(f"Job starting on fd {conn.fileno()}")
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
                        _log(f"Processing queue result for fd {conn.fileno()}")
                        result = result_queue.get(block=True)
                        _log(f"got result on fd {conn.fileno()}")

                        if result is not None:
                            _log(f"Sending result on fd {conn.fileno()}")
                            conn.send(result)
                            _log(f"Sent result for process {p.pid} on fd {conn.fileno()}")
                    except Exception as e:
                        _log(f"failed to get result from queue: {str(e)} on fd {conn.fileno()}")
                elif not p.is_alive():
                    # Send an empty message signaling failure
                    errmsg = mp.pack(f"Process {p.pid} on fd {conn.fileno()} not alive and no result available", "s")
                    error_packet = _make_data(errmsg, status = PACKET_STATUS_FAIL)
                    conn.send(error_packet)
                else:
                    # the process is still alive and no data has been reveived
                    # so we continue to wait
                    continue

                try:
                    # Process is done or we got a result, clean up
                    _log(f"Closing connection for process {p.pid} on fd {conn.fileno()}")
                    conn.close()
                    _log(f"Removing job for process {p.pid} on fd {conn.fileno()}")
                    queue.remove(job)
                    _log(f"Joining process {p.pid} on fd {conn.fileno()}")
                    p.join(timeout=0.0001)  # Wait for up to 1ms
                    if p.is_alive():
                        _log(f"Force terminating process {p.pid} on fd {conn.fileno()}")
                        p.terminate()
                        p.join(timeout=0.0001)  # Wait again to ensure termination
                    _log(f"Finished handling process {p.pid} on fd {conn.fileno()}")
                except Exception as e:
                    _log(f"failed to cleanup properly: {str(e)} on fd {conn.fileno()}")


if __name__ == "__main__":
    try:
        socket_path = sys.argv[1]
        global_state["tmpdir"] = sys.argv[2]
        server(socket_path)
    except Exception as e:
        _log(f"Python pool failed: {str(e)}")
        sys.exit(1)
