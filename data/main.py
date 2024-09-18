def message_response(data):
    args = data.split(" ")
    cmdID = int(args[0])
    arg_files = args[1:]

    _log(f"dispatching on {str(cmdID)}")

    try:
        mlc_function = dispatch[cmdID]
    except KeyError:
        sys.exit(
            "Internal error in python pool: no manifold found with id={}".format(cmdID)
        )

    result = mlc_function(*arg_files)

    _log(f"from cmdID {str(cmdID)} pool returning message '{_max_string(str(result))}'")

    return result


def worker(data, result_queue):
    try:
        _log(f"Worker started with data {_max_string(data)}")
        result = message_response(data.decode())
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
                    _log(f"Job {str(conn)} starting with data '{_max_string(data)}'")
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
                            conn.send(result.encode())
                            _log(f"Sent result for process {p.pid}")
                    except Exception as e:
                        _log(f"failed to get result from queue: {str(e)}")
                elif not p.is_alive():
                    _log(f"Process {p.pid} not alive and no result available")
                    # Send an empty message signaling failure
                    conn.send("".encode())
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

            time.sleep(0.001)


if __name__ == "__main__":
    socket_path = sys.argv[1]
    server(socket_path)
