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
            sys.exit(
                "Internal error in python pool: no manifold found with id={}".format(cmdID)
            )

        mlc_function = dispatch[cmdID]

        try:
            passing_result = mlc_function(*args)
            result = _pack(
                "32s{}s".format(len(passing_result)),
                _make_header(
                    length = len(passing_result),
                    command = _pack("Bxxxxxxx", PACKET_RETURN_PASS)
                ),
                passing_result
            )
        except Exception as e:
            failing_result = str(e).encode("utf8")
            result = _pack(
                "32s{}s".format(len(failing_result)),
                _make_header(
                    length = len(failing_result),
                    command = _pack("Bxxxxxxx", PACKET_RETURN_FAIL)
                ),
                failing_result
            )

        _log(f"from cmdID {str(cmdID)} pool returning message '{len(result)}'")
    else:
        errmsg = "Expected a call packet, found {str(msg_cmd_type)}"
        _log(errmsg)
        raise ValueError(errmsg)

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
        _log(f"Worker error: {e}")
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
                conn, addr = s.accept()
                _log(f"Connected by {addr}")
                data = conn.recv(BUFFER_SIZE)

                if data:
                    _log(f"Job {str(conn)} starting")
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
                        _log("Processing queue result")
                        result = result_queue.get(block=True)
                        _log("got result")

                        if result is not None:
                            _log(f"Sending result")
                            conn.send(result)
                            _log(f"Sent result for process {p.pid}")
                    except Exception as e:
                        _log(f"failed to get result from queue: {str(e)}")
                elif not p.is_alive():
                    _log(f"Process {p.pid} not alive and no result available")
                    # Send an empty message signaling failure
                    conn.send(b'fuck')
                else:
                    # the process is still alive and no data has been reveived
                    # so we continue to wait
                    continue

                try:
                    # Process is done or we got a result, clean up
                    _log(f"Closing connection for process {p.pid}")
                    conn.close()
                    _log(f"Removing job for process {p.pid}")
                    queue.remove(job)
                    _log(f"Joining process {p.pid}")
                    p.join(timeout=0.0001)  # Wait for up to 1ms
                    if p.is_alive():
                        _log(f"Force terminating process {p.pid}")
                        p.terminate()
                        p.join(timeout=0.0001)  # Wait again to ensure termination
                    _log(f"Finished handling process {p.pid}")
                except Exception as e:
                    _log(f"failed to cleanup properly: {str(e)}")


if __name__ == "__main__":
    socket_path = sys.argv[1]
    server(socket_path)

