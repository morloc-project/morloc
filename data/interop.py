def _get_value(data: bytes) -> str:

    (cmd, offset, length) = _read_header(data)

    cmd_type = cmd[0]

    data_start = 32 + offset

    if(cmd_type != PACKET_TYPE_DATA):
        _log("You are fucked")

    cmd_source = cmd[1]
    cmd_format = cmd[2]

    if cmd_source == PACKET_SOURCE_MESG:
        if cmd_format == PACKET_FORMAT_JSON:
            return data[data_start:].decode()
        else:
            _log("Invalid format")
    elif cmd_source == PACKET_SOURCE_FILE:
        if cmd_format == PACKET_FORMAT_JSON:
            filename = data[data_start:].decode()
            with open(filename, 'r') as file:
                content = file.read()
            os.unlink(filename)  # Delete the temporary file
            return content
        else:
            _log("Invalid format")
    else:
        _log("Invalid source")

    return ""  # fail




# Interop section ####

PACKET_TYPE_DATA    = 0x00
PACKET_TYPE_CALL    = 0x01
PACKET_TYPE_CALLRET = 0x02
PACKET_TYPE_GET     = 0x03
PACKET_TYPE_GETRET  = 0x04
PACKET_TYPE_PUT     = 0x05
PACKET_TYPE_PUTRET  = 0x06
PACKET_TYPE_PING    = 0x07
PACKET_TYPE_PINGRET = 0x08

PACKET_SOURCE_MESG = 0x00 # the message contains the data
PACKET_SOURCE_FILE = 0x01 # the message is a path to a file of data
PACKET_SOURCE_NXDB = 0x02 # the message is a key to the nexus uses to access the data

PACKET_FORMAT_JSON = 0x00

PACKET_COMPRESSION_NONE = 0x00 # uncompressed

PACKET_ENCRYPTION_NONE  = 0x00 # unencrypted

PACKET_RETURN_PASS = 0x00
PACKET_RETURN_FAIL = 0x01


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

def _write_call_packet(mid: int, args: list[bytes]) -> bytes:
    header = _make_header(
      sum([len(arg) for arg in args]),
      command = _pack("BIxxx", PACKET_TYPE_CALL, mid)
    )

    # find the lenghts of all the arguments
    args_format = "".join(str(len(n)) + "s" for n in args)

    return _pack("32s" + args_format, header, *args)

def _put_value(value: bytes) -> bytes:
    """
    Takes an encoded value and returns a packet representing it. This packet may
    be read by another _get_value function to retrieve the encoded value.

    If the value is small, the value may be stored in the packet itself. If it
    is larger, then it may be stored in a database or file and the packet will
    be a key or filename needed to retrieve it.
    """

    if len(value) <= 65536 - 32:
        return _pack(
            "32s{}s".format(len(value)),
            _make_header(
                len(value),
                _pack("BBBBBxxx",
                    PACKET_TYPE_DATA,
                    PACKET_SOURCE_MESG,
                    PACKET_FORMAT_JSON,
                    PACKET_COMPRESSION_NONE,
                    PACKET_ENCRYPTION_NONE,
                )
            ),
            value
        )
    else:
        # for large data, write a temporary file
        with tempfile.NamedTemporaryFile(delete=False, mode='wb') as temp_file:
            temp_file.write(value)
            tmpfilename = temp_file.name

        return _pack(
            "32s{}s".format(len(value)),
            _make_header(
                len(tmpfilename),
                _pack("BBBBBxxx",
                    PACKET_TYPE_DATA,
                    PACKET_SOURCE_FILE,
                    PACKET_FORMAT_JSON,
                    PACKET_COMPRESSION_NONE,
                    PACKET_ENCRYPTION_NONE,
                )
            ),
            value
        )

def _get_value(data: bytes) -> bytes:

    (cmd, offset, _) = _read_header(data)

    cmd_type = cmd[0]

    data_start = 32 + offset

    value = b''

    if(cmd_type == PACKET_TYPE_DATA):

        cmd_source = cmd[1]
        cmd_format = cmd[2]

        if cmd_source == PACKET_SOURCE_MESG:
            if cmd_format == PACKET_FORMAT_JSON:
                return data[data_start:]
            else:
                _log("Invalid format")
        elif cmd_source == PACKET_SOURCE_FILE:
            if cmd_format == PACKET_FORMAT_JSON:
                filename = data[data_start:]
                with open(filename, 'rb') as file:
                    value = file.read()
            else:
                errmsg = "Invalid format" 
                _log(errmsg)
                raise ValueError(errmsg)
        else:
            errmsg = "Invalid source" 
            _log(errmsg)
            raise ValueError(errmsg)
    else:
        errmsg = "Expected a data packet"
        _log(errmsg)
        raise ValueError(errmsg)

    return value

def _request_from_socket(socket_path, message):
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
        _log(f"connecting to {socket_path}")
        s.connect(socket_path)
        _log(f"sending message '{message}' to {socket_path}")
        s.send(message)
        _log("requesting data")
        data = s.recv(BUFFER_SIZE)
        _log(f"data {data} received from {socket_path}")
    return data


def _morloc_foreign_call(pool_pipe, manifold_id, arg_keys):
    _log("Making foreign call")
    _log(f"pool_pipe={pool_pipe}")
    _log(f"manifold_id={str(manifold_id)}")
    _log(f"arg_keys={str(arg_keys)}")

    msg = " ".join([manifold_id] + arg_keys).encode("utf8")

    delay_time = INITIAL_RETRY_DELAY
    for attempt in range(MAX_RETRIES):
        try:
            _log(f"requesting data from {pool_pipe} ...")
            result_file = _request_from_socket(pool_pipe, msg)
            break
        except (FileNotFoundError, ConnectionRefusedError) as e:
            _log("failed")
            if attempt == MAX_RETRIES - 1:
                raise e
            else:
                time.sleep(delay_time)
                delay_time *= RETRY_MULTIPLIER

    _log(f"returning data in file {result_file}")

    return result_file
