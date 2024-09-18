class MorlocForeignCallError(Exception):
    pass


def _put_value(value):
    """
    Store a value in a persistant manner and return a key by which it can be retrieved
    """

    temp = tempfile.NamedTemporaryFile(prefix="morloc_py_", delete=False)

    with open(temp.name, "w") as fh:
        print(value, file=fh)

    return temp.name


def _get_value(key):
    """
    Use a key to retrieve a value
    """

    with open(key, "r") as fh:
        value = fh.read()

    return value


def _log(msg, logfile="log"):
    with open(logfile, "a+") as fh:
        print(f"Python3: {msg}", flush=True, file=fh)


def _max_string(msg, max_length=80):
    if len(msg) < max_length:
        return msg
    else:
        return msg[0:max_length] + f"...(n={len(msg)})"


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
