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

        _log(f"from cmdID {str(cmdID)} pool returning message '{len(result)}'")
    else:
        errmsg = "Expected a call packet, found {str(msg_cmd_type)}"
        raise FailingPacket(errmsg)

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
                conn, addr = s.accept()
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
                        _log("Processing queue result for fd {conn.fileno()}")
                        result = result_queue.get(block=True)
                        _log("got result on fd {conn.fileno()}")

                        if result is not None:
                            _log(f"Sending result on fd {conn.fileno()}")
                            conn.send(result)
                            _log(f"Sent result for process {p.pid} on fd {conn.fileno()}")
                    except Exception as e:
                        _log(f"failed to get result from queue: {str(e)} on fd {conn.fileno()}")
                elif not p.is_alive():
                    # Send an empty message signaling failure
                    errmsg = msgpack.packb(f"Process {p.pid} on fd {conn.fileno()} not alive and no result available")
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
        server(socket_path)
        _log("Python pool exiting successfully")
        sys.exit(0)
    except Exception as e:
        _log(f"Python pool failed: {str(e)}")
        sys.exit(1)
