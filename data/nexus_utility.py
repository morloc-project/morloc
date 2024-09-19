#!/usr/bin/env python3

import json
import socket
import subprocess
import sys
import os
import tempfile
import signal
import time
import threading
import queue

# These three parameters describe the retru times for a pool connection to
# open. The default parameters sum to a 4s max wait, which is well beyond what
# it should take for any interpreter to fire up.
INITIAL_RETRY_DELAY = 0.001
RETRY_MULTIPLIER = 1.25
MAX_RETRIES = 30

# buffer size for socket IPC
BUFFER_SIZE = 1024

# default pipe names, must match the signal naming conventions of the pools
PYTHON_PIPE = "python3-pipe"
CPP_PIPE = "cpp-pipe"
R_PIPE = "r-pipe"

# Resources that need to be cleaned up when the session ends
resources = {"pools": {}, "files": []}


class Pool:
    def __init__(self, lang, process, pipe, stderr_queue, exit_status_queue):
        self.lang = lang
        self.process = process
        self.pipe = pipe
        self.stderr_queue = stderr_queue
        self.exit_status_queue = exit_status_queue


def _log(msg):
    with open("log", "a+") as fh:
        print(f"Nexus: {msg}", file=fh)


def cleanup():
    pools = resources["pools"]
    files = resources["files"]

    _log(f"Cleaning up")
    for pool in pools.values():
        if pool.process.poll() is None:  # If process is still running
            pool.process.terminate()  # Send SIGTERM
            try:
                pool.process.wait(timeout=5)  # Wait for up to 5 seconds
            except subprocess.TimeoutExpired:
                pool.process.kill()  # If still running, send SIGKILL

        # remove the socket pipe file
        try:
            os.unlink(pool.pipe)
        except:
            # this socket file may have been cleaned up by the pool
            pass

    # clean up temporary files and sockets
    for is_temp, file in files:
        if is_temp:
            os.unlink(file)


def clean_exit(exit_code, msg=""):
    cleanup()
    if msg:
        print(msg, file=sys.stderr)
    sys.exit(exit_code)


def signal_handler(sig, frame):
    clean_exit(1)


# Register signal handler for ctrl-c
# This avoids dumping a trace when a user kills a job
signal.signal(signal.SIGINT, signal_handler)


def start_language_server(lang, cmd, pipe):
    # Create a queue to store stderr output
    stderr_queue = queue.Queue()

    # Create a queue to store the exit status
    exit_status_queue = queue.Queue()

    # Function to read stderr and put it in the queue
    def read_stderr(process, queue):
        for line in process.stderr:
            queue.put(line)
        process.stderr.close()

    # Function to monitor process and get exit status
    def monitor_process(process, queue):
        exit_status = process.wait()
        queue.put(exit_status)

    # Start the language server in the background
    _log(f"Starting server with {cmd} ...")
    process = subprocess.Popen(
        cmd, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True
    )
    _log(f"Server started")

    # Start thread to read stderr
    stderr_thread = threading.Thread(target=read_stderr, args=(process, stderr_queue))
    stderr_thread.daemon = True
    stderr_thread.start()

    # Start thread to monitor process and get exit status
    monitor_thread = threading.Thread(
        target=monitor_process, args=(process, exit_status_queue)
    )
    monitor_thread.daemon = True
    monitor_thread.start()

    pool = Pool(lang, process, pipe, stderr_queue, exit_status_queue)

    resources["pools"][lang] = pool

    return pool


def client(pool, message):

    _log("entering nexus client")

    # it may take awhile for the pool to initialize and create the socket
    delay_time = INITIAL_RETRY_DELAY
    for attempt in range(MAX_RETRIES):
        # check to see if the pool has died
        if not pool.exit_status_queue.empty():
            exit_status = pool.exit_status_queue.get()
            print(
                f"{pool.lang} pool ended early with exit status {exit_status} and the error message:",
                file=sys.stderr,
            )
            while not pool.stderr_queue.empty():
                print(pool.stderr_queue.get(), file=sys.stderr, end="")
            clean_exit(1, "")
        # try to connect to the pool server
        try:
            _log("contacting pool ...")
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as s:
                _log("connecting to pool ...")
                s.connect(pool.pipe)
                _log(f"sending message: {message}")
                s.sendall(message)
                _log(f"waiting for response")
                data = s.recv(BUFFER_SIZE)
                _log(f"response received: {data}")
            break
        # try try again
        except (FileNotFoundError, ConnectionRefusedError) as e:
            _log("waiting for pool")
            if attempt == MAX_RETRIES - 1:
                raise e
            else:
                time.sleep(delay_time)
                delay_time *= RETRY_MULTIPLIER

    _log("exiting nexus client")

    return data.decode()


def as_file(input_str):
    if os.path.isfile(input_str):
        return (False, input_str)
    else:
        x = tempfile.NamedTemporaryFile(prefix="morloc_nexus_", delete=False)
        with open(x.name, "w") as fh_temp:
            if os.path.exists(input_str):
                with open(input_str, "r") as fh_sub:
                    print(fh_sub.read().strip(), file=fh_temp)
            else:
                try:
                    input_json = json.loads(input_str)
                    print(json.dumps(input_json), file=fh_temp)
                except json.JSONDecodeError:
                    clean_exit(1, "Invalid input '{input_str}'")
        return (True, x.name)


def run_command(mid, args, pool_lang, sockets):
    if len(args) != 0:
        clean_exit("Expected 0 arguments to 'pfoo', given " + str(len(args)))

    arg_files = []
    result = None
    error = ""

    try:
        # Start language servers
        for (lang, cmd, pipe) in sockets:
            try:
                start_language_server(lang, cmd, pipe)
            except Exception as e:
                _log(f"Failed to start {lang} language server: {str(e)}")
                raise  # Re-raise the exception to be caught in the outer try block

        # Store all inputs as files
        for arg in args:
            new_file = as_file(arg)
            arg_files.append(new_file)
            resources["files"].append(new_file)

        # Prepare pool message
        message = (" ".join([mid] + [x[1] for x in arg_files])).encode("utf8")

        # Send arguments over the socket
        try:
            result = client(resources["pools"][pool_lang], message)
        except Exception as e:
            _log(f"Error in client call: {str(e)}")
            raise  # Re-raise the exception to be caught in the outer try block

    except Exception as e:
        _log(f"An error occurred: {str(e)}")
        # You might want to set a specific error value or re-raise the exception
        # depending on how you want to handle errors at a higher level
        error = str(e)

    # Ensure that processes are stopped and files deleted in all cases
    finally:
        cleanup()

    # Print the JSON representation of the final value
    if result is not None:
        with open(result, "r") as fh:
            print(fh.read(), end="")
        clean_exit(0)
    else:
        clean_exit(1, error)
