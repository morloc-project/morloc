# Serialization section ####

def mlc_list(arg):
    """
  Helper function for building list types

  The "list" here is a homogenous vector, like the Haskell or lisp list, not
  the named, heterogenous lists of python. So `mlc_list(mlc_integer)` would be
  the python3 version of the Haskell type `[Int]`.

  @param args The type parameter for a list
  """
    return ("list", arg)


def mlc_tuple(*args):
    """
  Helper function for building tuple types

  @param *args The type parameter for the tuple
  """
    return ("tuple", [*args])


def mlc_record(**kwargs):
    """
  Helper function for building record types

  @param **kwargs The keyword arguments for the record
  """
    return ("record", dict(**kwargs))


def mlc_object(f, **kwargs):
    """
  Helper function for building object types

  @param **kwargs The keyword arguments for the record
  """
    return (f, dict(**kwargs))


mlc_int = ("int", None)

mlc_float = ("float", None)

mlc_str = ("str", None)

mlc_bool = ("bool", None)

mlc_null = ("null", None)

def serialize_list(x, schema):
    f = dispatch_serialize[schema[0]]
    return "[{}]".format(",".join([f(y, schema[1]) for y in x]))


def serialize_tuple(x, schema):
    elements = []
    for (t, e) in zip(schema, x):
        f = dispatch_serialize[t[0]]
        elements.append(f(e, t[1]))
    return "[{}]".format(",".join(elements))


def serialize_record(x, schema):
    entries = []
    for (k, t) in schema.items():
        try:
            f = dispatch_serialize[t[0]]
            entries.append('"{}":{}'.format(k, f(x[k], t[1])))
        except:
            print(f"Mismatch found between serial specification and actual serialized data.", file = sys.stderr)
            print(f"This may be caused by a sourced function that is not following its type signature.", file = sys.stderr)
            print(f"  k ({type(k)}): {str(k)}", file = sys.stderr)
            print(f"  t ({type(t)}): {str(t)}", file = sys.stderr)
            print(f"  x ({type(x)}): {str(x)}", file = sys.stderr)
            sys.exit(1)
    return "{{{}}}".format(",".join(entries))


def serialize_float(x, schema):
    return str(x)


def serialize_int(x, schema):
    return str(x)


def serialize_str(x, schema):
    return json.dumps(x)


def serialize_bool(x, schema):
    return json.dumps(x)

def serialize_none(x, schema):
    return json.dumps(None)

dispatch_serialize = { 
    "list" : serialize_list,
    "tuple" : serialize_tuple,
    "record" : serialize_record,
    "dict" : serialize_record,
    "float" : serialize_float,
    "int" : serialize_int,
    "str" : serialize_str,
    "bool" : serialize_bool,
    "None" : serialize_none,
  }


def mlc_serialize(x, schema):
    if type(schema[0]) == str:
        return dispatch_serialize[schema[0]](x, schema[1])
    else:
        # Is the label is not a string, then it is a constructor,
        # so the data should an object
        return serialize_record(x.__dict__, schema[1])


def deserialize_list(xs, schema):
    deserialize = dispatch_deserialize[schema[0]]
    return [deserialize(x, schema[1]) for x in xs]


def deserialize_tuple(xs, schema):
    return tuple([dispatch_deserialize[s[0]](x, s[1])  for (x,s) in zip(xs, schema)])


def deserialize_record(d0, schema):
    d = dict()
    for (k, v) in schema.items():
        deserializer = dispatch_deserialize[v[0]]
        d[k] = deserializer(d0[k], v[1])
    return d


dispatch_deserialize = {
    "list"   : deserialize_list,
    "tuple"  : deserialize_tuple,
    "record" : deserialize_record,
    "dict"   : deserialize_record,
    "float"  : lambda x, _: x,
    "int"    : lambda x, _: x,
    "str"    : lambda x, _: x,
    "bool"   : lambda x, _: x,
    "None"   : None
}


def mlc_deserialize(json_str, schema):
    try:
        x = json.loads(json_str)
    except json.JSONDecodeError as e:
        print(f"Python deserialization error in pymorlocinternals. Failed to deserialized type {type(json_str)} with value: {str(json_str)}", file=sys.stderr)
        print(f"Using schema: {str(schema)}", file=sys.stderr)
        print(f"JSONDecodeError: {str(e)}", file=sys.stderr)
        sys.exit(1)
    except:
        print(f"Failed to deserialize '{json_str}' of type '{type(json_str)}'", file=sys.stderr)
        sys.exit(1)
    if type(schema[0]) == str:
        return dispatch_deserialize[schema[0]](x, schema[1])
    else:
        return schema[0](**deserialize_record(x, schema[1]))


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
    fmt    = PACKET_FORMAT_JSON,
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
            errmsg = errmsg.encode("utf8")
        except:
            pass
        self.packet = _make_data(errmsg, status = PACKET_STATUS_FAIL)
        self.error_code = error_code
        super().__init__(self.packet)

    def __str__(self):
        errmsg = self.packet[31:].decode()
        if self.error_code:
            return f"FailingPacket ({self.error_code}): {errmsg}"
        return f"FailingPacket: {errmsg}"



def _put_value(value: bytes) -> bytes:
    """
    Takes an encoded value and returns a packet representing it. This packet may
    be read by another _get_value function to retrieve the encoded value.

    If the value is small, the value may be stored in the packet itself. If it
    is larger, then it may be stored in a database or file and the packet will
    be a key or filename needed to retrieve it.
    """

    if len(value) <= 65536 - 32:
        return _make_data(value)
    else:
        # for large data, write a temporary file
        with tempfile.NamedTemporaryFile(delete=False, mode='wb') as temp_file:
            temp_file.write(value)
            tmpfilename = temp_file.name
        return _make_data(tmpfilename.encode("utf8"), src=PACKET_SOURCE_FILE)

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
                raise FailingPacket("Invalid format")
        elif cmd_source == PACKET_SOURCE_FILE:
            if cmd_format == PACKET_FORMAT_JSON:
                filename = data[data_start:]
                with open(filename, 'rb') as file:
                    value = file.read()
            else:
                errmsg = "Invalid format" 
                raise FailingPacket(errmsg)
        else:
            errmsg = "Invalid source" 
            raise FailingPacket(errmsg)
    else:
        errmsg = "Expected a data packet"
        raise FailingPacket(errmsg)

    return value

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
