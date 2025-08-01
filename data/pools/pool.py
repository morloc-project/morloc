import signal
import sys
import select
import os # required for setting path to morloc dependencies
import time
from collections import OrderedDict
from multiprocessing import Process, cpu_count, Pipe, Value
from multiprocessing.reduction import recv_handle, send_handle
import functools


# Global variables for clean signal handling
daemon = None
workers = []
global_state = dict()

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

        if(morloc.is_local_call(client_data)):
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            try:
                result = dispatch[mid](*args)
            except Exception as e:
                result = morloc.make_fail_packet(str(e))

        elif(morloc.is_remote_call(client_data)):
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            try:
                result = remote_dispatch[mid](*args)
            except Exception as e:
                result = morloc.make_fail_packet(str(e))

        elif(morloc.is_ping(client_data)):
            result = morloc.pong(client_data)

        else:
            raise ValueError("Expected a ping or call type packet")

        morloc.send_packet_to_foreign_server(client_fd, result)

    except Exception as e:
        # this may be OK, a broken pipe will occur if a ping times out, in which
        # case a new ping will be sent
        print(f"job failed: {e!s}", file=sys.stderr)
    finally:
        # close child copy
        morloc.close_socket(client_fd)


def worker_process(pipe, shm_basename, shutdown_flag):
    morloc.shinit(shm_basename, 0, 0xffff)
    pipe_fd = pipe.fileno()  # Get the file descriptor
    while not shutdown_flag.value:
        # Wait until pipe is readable or shutdown is requested
        rlist, _, _ = select.select([pipe_fd], [], [], 0.1)
        if shutdown_flag.value:
            break
        if rlist:
            try:
                client_fd = recv_handle(pipe)
                run_job(client_fd)
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
        except Exception as e:
            print(f"In python daemon, failed to connect to client: {e!s}", file=sys.stderr)
            continue

        if client_fd >= 0:
            try:
                # Round-robin distribution to workers
                pipe = worker_pipes[current_worker]

                send_handle(pipe, client_fd, os.getpid())

                current_worker = (current_worker + 1) % len(worker_pipes)
            except Exception as e:
                print(f"In python daemon, failed to start worker: {e!s}", file=sys.stderr)
            finally:
                # close parent copy
                morloc.close_socket(client_fd)



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

    global_state["tmpdir"] = tmpdir

    # Create worker pool
    num_workers = max(1, cpu_count() - 1)
    workers = []
    worker_pipes = []  # Store worker-specific pipes

    for i in range(num_workers):
        # Create separate pipe for each worker
        parent_conn, child_conn = Pipe()
        worker = Process(target=worker_process, args=(parent_conn, shm_basename, shutdown_flag))
        worker.start()
        workers.append(worker)
        worker_pipes.append(child_conn)  # Store listener-side ends

    # Start client listener process
    listener_process = Process(
        target=client_listener,
        args=(worker_pipes, socket_path, tmpdir, shm_basename, shutdown_flag)  # Pass ALL pipes
    )
    listener_process.start()

    while not shutdown_flag.value:
        time.sleep(0.1)  # Keep main thread alive and responsive to signals

    # Shutdown sequence

    # 1. Stop listener first
    listener_process.terminate()
    listener_process.join(timeout=0.01)
    listener_process.kill()
    listener_process.join()  # Final blocking reap
    listener_process.close()

    # 2. Drain and close worker pipes
    for pipe in worker_pipes:
        while pipe.poll():
            try:
                pipe.recv()  # Clear buffers
            except (EOFError, OSError):
                break
        pipe.close()

    # 3. Terminate workers with escalating force
    for p in workers:
        if p.is_alive():
            p.kill()
        p.join()  # Final blocking reap
        p.close()

    # 4. Clean shared resources
    sys.exit(0)
