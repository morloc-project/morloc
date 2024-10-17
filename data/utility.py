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
            errmsg = msgpack.packb(errmsg.encode("utf8"))
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
            errmsg = msgpack.unpackb(self.packet[32:])
        except Exception as e:
            return f"Malformed FailingPacket {str(e)}"
        if self.error_code:
            return f"FailingPacket ({self.error_code}): {errmsg}"
        return f"FailingPacket: {errmsg}"


# Custom encoder for tuples
def encode_tuple(obj):
    if isinstance(obj, tuple):
        return msgpack.ExtType(MSGPACK_TYPE_TUPLE, msgpack.packb(list(obj), default=encode_tuple, strict_types = True))
    return obj

# Custom decoder for tuples
def decode_tuple(code, data):
    if code == MSGPACK_TYPE_TUPLE:
        return tuple(msgpack.unpackb(data, ext_hook=decode_tuple))
    return msgpack.ExtType(code, data)

def msgpack_serialize(data):
    return msgpack.packb(data, default=encode_tuple, strict_types = True)

def msgpack_deserialize(data):
    return msgpack.unpackb(data, ext_hook=decode_tuple)


def _get_value(data: bytes):

    (cmd, offset, _) = _read_header(data)

    cmd_type   = cmd[0]
    cmd_source = cmd[1]
    cmd_format = cmd[2]

    if(cmd_type != PACKET_TYPE_DATA):
        errmsg = "Expected a data packet"
        raise FailingPacket(errmsg)

    if cmd_format == PACKET_FORMAT_MSGPACK:
        deserializer = msgpack_deserialize
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
    elif cmd_source == PACKET_SOURCE_FILE:
        try:
            filename = data[data_start:].decode()
            with open(filename, 'rb') as file:
                return(deserializer(file.read()))
        except Exception as e:
            raise FailingPacket(f"Failed to parse file packet: {str(e)}")
    else:
        errmsg = "Invalid source" 
        raise FailingPacket(errmsg)

def _put_value(value) -> bytes:
    """
    Takes an encoded value and returns a packet representing it. This packet may
    be read by another _get_value function to retrieve the encoded value.

    If the value is small, the value may be stored in the packet itself. If it
    is larger, then it may be stored in a database or file and the packet will
    be a key or filename needed to retrieve it.
    """

    try:
        data = msgpack_serialize(value)
    except Exception as e:
        raise FailingPacket(f"Could not serialize data: {str(e)}")

    if len(data) <= 65536 - 32:
        return _make_data(data)
    else:
        # for large data, write a temporary file
        with tempfile.NamedTemporaryFile(delete=False, mode='wb') as temp_file:
            temp_file.write(data)
            tmpfilename = temp_file.name
        return _make_data(tmpfilename.encode("utf8"), src=PACKET_SOURCE_FILE)

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
