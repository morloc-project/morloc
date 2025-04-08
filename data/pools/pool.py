import signal
import sys
import os # required for setting path to morloc dependencies
import time
from multiprocessing import Process, cpu_count, Pipe, Value
from multiprocessing.reduction import recv_handle, send_handle


# Global variables for clean signal handling
daemon = None
workers = []

# AUTO include imports start
# <<<BREAK>>>
# AUTO include imports end

import pymorloc as morloc

# AUTO include serialization start
# <<<BREAK>>>
# AUTO include serialization end


# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end


# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end

def run_job(client_fd: int) -> None:
    try:
        client_data = morloc.stream_from_client(client_fd)

        if(morloc.is_ping(client_data)):
            result = morloc.pong(client_data)

        elif(morloc.is_call(client_data)):
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            result = dispatch[mid](*args)

        else:
            raise ValueError("Expected a ping or call type packet")

        morloc.send_packet_to_foreign_server(client_fd, result)

    except Exception as e:
        print(f"job failed: {e!s}", file=sys.stderr)
    finally:
        morloc.socket_close(client_fd)


def worker_process(worker_id, pipe, shm_basename, shutdown_flag):
    morloc.shinit(shm_basename, 0, 0xffff)
    while not shutdown_flag.value:
        try:
            # Receive duplicated FD valid in this process
            client_fd = recv_handle(pipe)

            run_job(client_fd) # Process with original FD
        except (EOFError, OSError):
            break


def signal_handler(sig, frame):
    shutdown_flag.value = True
    if daemon is not None:
        morloc.close_daemon(daemon)


def client_listener(worker_pipes, socket_path, tmpdir, shm_basename, shutdown_flag):
    global daemon
    daemon = morloc.start_daemon(socket_path, tmpdir, shm_basename, 0xffff)
    current_worker = 0  # Round-robin counter

    while not shutdown_flag.value:
        try:
            client_fd = morloc.wait_for_client(daemon)

            if client_fd >= 0:
                # Round-robin distribution to workers
                pipe = worker_pipes[current_worker]

                send_handle(pipe, client_fd, os.getpid())

                current_worker = (current_worker + 1) % len(worker_pipes)
        except Exception as e:
            print(f"Listener error: {e!s}", file=sys.stderr)


if __name__ == "__main__":
    shutdown_flag = Value('b', False)  # Shared flag

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Process arguments passed from the nexus
    try:
        socket_path = sys.argv[1]
        tmpdir = sys.argv[2]
        shm_basename = sys.argv[3]
    except IndexError:
        print("Usage: script.py <socket_path> <tmpdir> <shm_basename>")
        sys.exit(1)

    # Create worker pool
    num_workers = cpu_count()
    workers = []
    worker_pipes = []  # Store worker-specific pipes

    for i in range(num_workers):
        # Create separate pipe for each worker
        parent_conn, child_conn = Pipe()
        worker = Process(target=worker_process, args=(i, parent_conn, shm_basename, shutdown_flag))
        worker.start()
        workers.append(worker)
        worker_pipes.append(child_conn)  # Store listener-side ends

    # Start client listener process
    listener_process = Process(
        target=client_listener,
        args=(worker_pipes, socket_path, tmpdir, shm_basename, shutdown_flag)  # Pass ALL pipes
    )
    listener_process.start()

    while not shutdown_flag:
        time.sleep(0.1)  # Keep main thread alive and responsive to signals

    # Terminate listener process
    if(listener_process):
        listener_process.join(0.1)
        listener_process.terminate()

    # Terminate worker processes
    for p in workers:
        if p is not None:
            if p.is_alive():
                p.join(0.1)  # Wait for clean exit
                if p.is_alive():
                    p.terminate()

    sys.exit(0)
