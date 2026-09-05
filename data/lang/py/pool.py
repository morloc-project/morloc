import signal
import sys
import select
import os # required for setting path to morloc dependencies
import time
import copy
import array
import struct
import socket as _socket
import threading
import queue
from collections import OrderedDict
from multiprocessing import Process, Value, RawValue
import ctypes
import functools
import importlib.util


# Global variables for clean signal handling
daemon = None
workers = []
global_state = dict()
_shutdown_wakeup_fd = -1

# The language preamble (runtime bootstrap: `import pymorloc as morloc`, path
# setup) and the generated schema/closure tables run at module top, in the
# coordinator parent -- they are thread-free and the parent depends on them.
# AUTO include preamble start
# <<<BREAK>>>
# AUTO include preamble end

# User `source` imports, by contrast, are deferred out of the parent and run in
# each worker AFTER fork (see _mlc_load_user_sources). Importing heavy libraries
# in the parent and then forking is unsafe on macOS: e.g. numpy loads Apple's
# Accelerate/libdispatch, which spins background threads, and forking a
# multithreaded process then deadlocks/aborts in the child (fork-after-threads).
# Deferring keeps the parent single-threaded, and each worker initializes its
# libraries in its own process -- preserving full per-worker thread parallelism.
# The block is held verbatim in a raw string and exec'd into module globals (so
# the imports land at module scope exactly as if run at import time, past fork).
_mlc_user_sources = r'''
# AUTO include user-sources start
# <<<BREAK>>>
# AUTO include user-sources end
'''

_mlc_sources_loaded = False
_mlc_source_error = None

def _mlc_import_source(module_path):
    # Load a `source`d file by location rather than by module name.
    #
    # A user file may be named after a module that is already in sys.modules --
    # `copy` and `time` are imported above, and they pull in others such as
    # `heapq` transitively -- and importing by name returns that module instead
    # of the user's file, so none of their functions are found. The set is not
    # one a user can be expected to know, so resolve the file on the search path
    # and load it from there.
    #
    # The module is registered under a reserved key, so it neither reads nor
    # replaces a real module of the same name: a plain `import copy` from inside
    # a user file still reaches the standard library.
    rel = module_path.replace(".", os.sep) + ".py"
    for root in sys.path:
        candidate = os.path.join(root, rel)
        if os.path.isfile(candidate):
            key = "_mlc_src_" + module_path.replace(".", "_")
            spec = importlib.util.spec_from_file_location(key, candidate)
            module = importlib.util.module_from_spec(spec)
            sys.modules[key] = module
            spec.loader.exec_module(module)
            return module
    # Nothing on the search path: an installed package, imported by name.
    return importlib.import_module(module_path)


def _mlc_load_user_sources():
    # Idempotent; called once per worker after shinit (and by --health). Any
    # failure is captured in _mlc_source_error so run_job can return it as a
    # clean fail packet rather than dying and masquerading as "connection closed".
    global _mlc_sources_loaded, _mlc_source_error
    if _mlc_sources_loaded:
        return
    # Import-time stdout (package banners/prints) must not corrupt the data
    # stream shared with the nexus; route it to stderr for the duration.
    _saved_stdout = sys.stdout
    sys.stdout = sys.stderr
    try:
        exec(compile(_mlc_user_sources, "<morloc-sources>", "exec"), globals())
        _mlc_sources_loaded = True
    except BaseException as e:
        import traceback
        _mlc_source_error = f"failed to load pool sources: {e!s}\n{traceback.format_exc()}"
        # Surface the failure to stderr immediately at load time. The pool stays
        # up and returns this as a fail packet per call (the deferred-loading
        # contract), and the readiness ping still succeeds because the process
        # and its IPC are alive -- but without this print the cause would be
        # invisible until the first call. Full import validation is --health's
        # job (it exits non-zero on this same error).
        print(_mlc_source_error, file=sys.stderr)
        sys.stderr.flush()
    finally:
        sys.stdout = _saved_stdout

# Dynamic worker spawning: monkey-patch foreign_call to track busy workers.
# Workers atomically increment busy_count before a foreign_call and decrement
# after. When busy_count reaches total_workers, a byte is written to a wake-up
# pipe to tell the main process to spawn a new worker.
_original_foreign_call = morloc.foreign_call
_busy_ref = None
_total_ref = None
_wakeup_fd = -1

def _init_worker_tracking(busy, total, wakeup_fd):
    global _busy_ref, _total_ref, _wakeup_fd
    _busy_ref = busy
    _total_ref = total
    _wakeup_fd = wakeup_fd
    morloc.foreign_call = _tracked_foreign_call

def _tracked_foreign_call(*args):
    prev = _busy_ref.value
    _busy_ref.value = prev + 1
    if prev + 1 >= _total_ref.value and _wakeup_fd >= 0:
        try:
            os.write(_wakeup_fd, b'!')
        except OSError:
            pass
    try:
        return _original_foreign_call(*args)
    finally:
        _busy_ref.value -= 1

def __mlc_wrap_log(group, start_tmpl, pass_tmpl, fail_tmpl, fn):
    def go(*args):
        call_id = morloc.log_next_id()
        t0 = time.monotonic()
        if start_tmpl is not None:
            morloc.log_emit(start_tmpl, group, 0.0, call_id)
        try:
            r = fn(*args)
            if pass_tmpl is not None:
                morloc.log_emit(pass_tmpl, group, time.monotonic() - t0, call_id)
            return r
        except BaseException:
            if fail_tmpl is not None:
                morloc.log_emit(fail_tmpl, group, time.monotonic() - t0, call_id)
            raise
    return go


# Defunctionalized-closure support. A morloc function value is a
# functools.partial over a manifold function m<mid>; when it crosses a language
# boundary it travels as the wire tuple (home_language, manifold_id,
# captured_packets) and is applied on the far side by calling back to this pool.

def mlc_reify(f, home_lang):
    # Recover (mid, captured) from a closure and serialize its captured values.
    # partial.func.__name__ is "m<mid>"; partial.args are the captured values in
    # the manifold's context-argument order; mlc_closure_table[mid] holds their
    # schemas.
    mid = int(f.func.__name__[1:])
    captured = list(f.args)
    cap_schemas = mlc_closure_table.get(mid, [])
    packets = [morloc.put_value(c, s) for c, s in zip(captured, cap_schemas)]
    return (home_lang, mid, packets)


def mlc_reflect_from_tuple(tup, arg_schemas, res_schema):
    # Rebuild a callable from an already-deserialized closure wire tuple
    # (home_lang, mid, captured_packets). On application it serializes its
    # arguments, appends them to the captured packets, and calls back to the
    # producing pool via foreign_call on the closure's manifold id. Used when the
    # closure is nested in an aggregate whose enclosing get_value has already
    # parsed the tuple.
    home_lang, mid, captured = tup
    sock = os.path.join(global_state["tmpdir"], "pipe-" + home_lang)
    def _call(*args):
        packets = list(captured) + [morloc.put_value(a, s) for a, s in zip(args, arg_schemas)]
        return morloc.get_value(morloc.foreign_call(sock, mid, packets), res_schema)
    return _call


def mlc_reflect(pkt, tuple_schema, arg_schemas, res_schema):
    # Rebuild a callable from a raw incoming closure wire packet: deserialize the
    # tuple, then reflect it. Used when the closure is the top-level crossing
    # value (the whole packet is the closure tuple).
    return mlc_reflect_from_tuple(morloc.get_value(pkt, tuple_schema), arg_schemas, res_schema)


def mlc_make_closure_dispatch(mid, arg_schemas, res_schema):
    # Serial dispatch wrapper for a closure manifold: deserialize the incoming
    # captured ++ bound argument packets, call the native manifold, and
    # serialize the result. Registered under the closure's mid so a foreign
    # apply reaches it.
    fn = globals()["m" + str(mid)]
    def _wrapper(*sargs):
        args = [morloc.get_value(s, sch) for s, sch in zip(sargs, arg_schemas)]
        return morloc.put_value(fn(*args), res_schema)
    return _wrapper


# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end


# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end


def _with_debug_trace(msg: str) -> str:
    # Concatenate the morloc debug trace (if --debug was compiled in
    # and any frames were recorded) with the raised exception's
    # message. Returns msg unchanged when no trace is present. Each
    # manifold catch also appends its own "  at <name> [py] (mid=...,
    # srcloc)" line to the message via string concat, so non-debug
    # tracebacks still compose across pools.
    trace = morloc.debug_drain_frames()
    return f"{msg}\n{trace}" if trace else msg


def run_job(client_fd: int) -> None:
    try:
        # Free SHM from previous dispatch result (consumed by caller)
        morloc.shm_tracker_flush()
        morloc.debug_flush_dispatch()
        client_data = morloc.stream_from_client(client_fd)

        # If deferred source loading failed, every call fails with that error
        # (the program cannot run without its imports). Report it as a clean
        # fail packet rather than a cryptic dispatch error.
        if _mlc_source_error is not None and not morloc.is_ping(client_data):
            sys.stdout.flush()
            morloc.send_packet_to_foreign_server(client_fd, morloc.make_fail_packet(_mlc_source_error))
            return

        if(morloc.is_local_call(client_data)):
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            try:
                result = dispatch[mid](*args)
            except Exception as e:
                result = morloc.make_fail_packet(_with_debug_trace(str(e)))

        elif(morloc.is_remote_call(client_data)):
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            try:
                result = remote_dispatch[mid](*args)
            except Exception as e:
                result = morloc.make_fail_packet(_with_debug_trace(str(e)))

        elif(morloc.is_ping(client_data)):
            # The nexus abandons a readiness-ping connection when its probe
            # times out (common for slow-starting pools like Python) and
            # retries on a fresh connection. Answering the orphaned ping then
            # fails with a broken pipe -- benign, so send the pong silently and
            # do not route a failure through the loud "job failed" handler (the
            # C++/Rust pools already ignore pong-send failures the same way).
            sys.stdout.flush()
            try:
                morloc.send_packet_to_foreign_server(client_fd, morloc.pong(client_data))
            except Exception:
                pass
            return

        else:
            raise ValueError("Expected a ping or call type packet")

        # Flush stdout BEFORE sending the result back. The nexus prints its
        # own output (the return value) right after receiving this response.
        # Both processes share the same stdout fd, so if we flush after sending,
        # the nexus can print first, causing out-of-order output.
        sys.stdout.flush()

        morloc.send_packet_to_foreign_server(client_fd, result)

    except Exception as e:
        # Try to send a fail packet back to the caller before giving up.
        # This may fail (e.g., broken pipe from a timed-out ping), which is OK.
        try:
            result = morloc.make_fail_packet(str(e))
            morloc.send_packet_to_foreign_server(client_fd, result)
        except Exception:
            pass
        print(f"job failed: {e!s}", file=sys.stderr)
    finally:
        # Reclaim any stdio singleton claim this dispatch left open (e.g. a
        # handler that raised past @close on a broken pipe). Runs on the
        # same worker thread that opened it, so the reclaim's call_id gate
        # matches. Without this a leaked @stdout claim wedges every later
        # open with "@stdout already open in this nexus".
        morloc.reclaim_stdio_after_dispatch()
        # Safety-net flush for any output from error handling paths
        sys.stdout.flush()
        # close child copy
        morloc.close_socket(client_fd)


def _send_fd(sock, fd):
    """Send a file descriptor over a Unix domain socket."""
    sock.sendmsg([b'\x00'],
                 [(_socket.SOL_SOCKET, _socket.SCM_RIGHTS,
                   array.array('i', [fd]))])

def _recv_fd(sock):
    """Receive a file descriptor from a Unix domain socket."""
    msg, ancdata, flags, addr = sock.recvmsg(1, _socket.CMSG_SPACE(4))
    if not msg and not ancdata:
        raise EOFError("Connection closed")
    for cmsg_level, cmsg_type, cmsg_data in ancdata:
        if (cmsg_level == _socket.SOL_SOCKET and
                cmsg_type == _socket.SCM_RIGHTS):
            a = array.array('i')
            a.frombytes(cmsg_data[:4])
            return a[0]
    raise RuntimeError("No fd received in ancillary data")


WORKER_IDLE_TIMEOUT = 5.0  # seconds before an idle worker exits

def worker_process(job_fd, tmpdir, shm_basename, shutdown_flag, busy_count, total_workers, wakeup_w):
    # Reset signal handlers inherited from main. If user code inside run_job
    # calls multiprocessing.Pool (or anything else that forks and later
    # SIGTERMs its own children), those grandchildren would otherwise inherit
    # main's signal_handler and flip the shared shutdown_flag, causing main
    # to SIGKILL this worker mid-response. See the multiprocessing-py-1 bug.
    signal.signal(signal.SIGTERM, signal.SIG_DFL)
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    morloc.set_fallback_dir(tmpdir)
    morloc.shinit(shm_basename, 0, 0xffff)
    # Load user sources HERE, post-fork, in the worker's own process (see the
    # _mlc_user_sources note). A failure is recorded, not raised: run_job turns
    # it into a fail packet so the caller gets the real import error.
    _mlc_load_user_sources()
    _init_worker_tracking(busy_count, total_workers, wakeup_w)
    sock = _socket.fromfd(job_fd, _socket.AF_UNIX, _socket.SOCK_STREAM)
    os.close(job_fd)  # sock owns a dup'd copy
    last_activity = time.monotonic()
    try:
        # poll() (not select.select) avoids the FD_SETSIZE=1024 ceiling: a
        # job-queue fd >= 1024 makes select.select raise ValueError and kill the
        # worker. The Rust/C/R pools already use poll(2) for this reason.
        poller = select.poll()
        poller.register(sock.fileno(), select.POLLIN)
        while not shutdown_flag.value:
            events = poller.poll(10)  # milliseconds (was 0.01s)
            if shutdown_flag.value:
                break
            if events:
                try:
                    client_fd = _recv_fd(sock)
                    run_job(client_fd)
                    last_activity = time.monotonic()
                except (EOFError, OSError):
                    break
            elif total_workers.value > 1 and time.monotonic() - last_activity > WORKER_IDLE_TIMEOUT:
                break
    except BaseException as e:
        # Catch-all for errors that escape run_job's own exception handling:
        # MemoryError, KeyboardInterrupt, SystemExit, or bugs in the worker
        # loop itself. Without this, the worker dies silently and the nexus
        # only sees "failed to read response header" with no indication of
        # what went wrong in the pool.
        #
        # Race condition: the nexus detects the broken socket and may start
        # its clean_exit tear-down (SIGTERM -> SIGKILL) while this print is
        # still buffered. We flush immediately to maximize the chance the
        # message reaches the terminal before we are killed. stderr is
        # line-buffered (set in __main__), but the flush is a safety net for
        # edge cases (redirected stderr, forked-process buffer state).
        import traceback
        print(f"morloc pool worker fatal error: {e!s}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        sys.stderr.flush()
    finally:
        sock.close()


def signal_handler(sig, frame):
    global daemon
    # Ignore further SIGTERM/SIGINT during cleanup. Python processes pending
    # signals between bytecodes, including while another signal handler is
    # running, so a second SIGTERM arriving mid-cleanup would otherwise
    # re-enter this handler and double-free the daemon pointer.
    try:
        signal.signal(signal.SIGTERM, signal.SIG_IGN)
        signal.signal(signal.SIGINT, signal.SIG_IGN)
    except Exception:
        pass
    shutdown_flag.value = True
    if _shutdown_wakeup_fd >= 0:
        try:
            os.write(_shutdown_wakeup_fd, b'!')
        except OSError:
            pass
    # Capture the daemon pointer into a local and clear the global BEFORE
    # invoking close_daemon. If a pending signal still slips through and
    # re-enters this handler, it will see daemon=None and skip the free.
    d = daemon
    daemon = None
    if d is not None:
        morloc.close_daemon(d)


def client_listener(job_fd, socket_path, tmpdir, shm_basename, shutdown_flag):
    global daemon
    daemon = morloc.start_daemon(socket_path, tmpdir, shm_basename, 0xffff)
    sock = _socket.fromfd(job_fd, _socket.AF_UNIX, _socket.SOCK_STREAM)
    os.close(job_fd)  # sock owns a dup'd copy

    while not shutdown_flag.value:
        try:
            client_fd = morloc.wait_for_client(daemon)
        except Exception as e:
            print(f"In python daemon, failed to connect to client: {e!s}", file=sys.stderr)
            continue

        if client_fd > 0:
            try:
                _send_fd(sock, client_fd)
            except Exception as e:
                print(f"In python daemon, failed to start worker: {e!s}", file=sys.stderr)
            finally:
                morloc.close_socket(client_fd)
    sock.close()



def _select_pool_mode():
    # Concurrency model for the Python pool:
    #   MORLOC_PY_POOL=fork    -> process workers (the fork model; works on Linux
    #                             and lets user code fork freely)
    #   MORLOC_PY_POOL=thread  -> thread workers (no live-interpreter fork; safe
    #                             on macOS, where forking a live CPython aborts)
    # Default: thread on macOS, fork elsewhere. The env var overrides the default
    # so BOTH models are exercisable on BOTH platforms (test the thread model on
    # Linux CI; force fork on macOS to reconfirm the crash).
    mode = os.environ.get("MORLOC_PY_POOL", "").strip().lower()
    if mode in ("fork", "thread"):
        return mode
    return "thread" if sys.platform == "darwin" else "fork"


def run_thread_pool(socket_path, tmpdir, shm_basename):
    # Thread-based pool: workers are threads, so a live CPython interpreter is
    # never forked (that abort is the macOS pool-death bug). Re-entrant callbacks
    # are served because foreign_call releases the GIL (pymorloc), letting a
    # sibling worker run while one is blocked on a downstream pool. Per-dispatch
    # state (shm_tracker, recur_env) is __thread in pymorloc, so workers do not
    # corrupt one another; libmorloc's own dispatch state is already thread-safe
    # (the C++ pool runs this same threaded model). User multi-threading/parallel
    # map still works: multiprocessing uses the 'fork' start method (forced in
    # __main__) so a user Pool worker inherits the manifolds exec'd into this
    # process's globals; spawn would re-import and fail to find them. Forking
    # this threaded pool is safe on macOS (OBJC_DISABLE_INITIALIZE_FORK_SAFETY;
    # the pool's threads hold no Apple-framework locks at fork time).
    #
    # Concurrency note: worker manifolds run on multiple threads sharing one
    # interpreter, so user Python source is expected to be thread-safe under
    # concurrent/re-entrant dispatch (morloc manifolds are pure functions, and
    # numpy/most C extensions release the GIL and are safe). Source that mutates
    # module-level globals from a manifold is the exception. This is inherent to
    # the thread model -- the process-isolated fork model is not available on
    # macOS (forking a live interpreter aborts), and serializing dispatch would
    # forfeit the required in-pool parallelism.
    morloc.set_fallback_dir(tmpdir)
    morloc.shinit(shm_basename, 0, 0xffff)  # attach SHM once for the process
    _mlc_load_user_sources()  # no fork on this path -> safe to import in-thread

    daemon = morloc.start_daemon(socket_path, tmpdir, shm_basename, 0xffff)

    stop = threading.Event()
    def _on_signal(_sig, _frame):
        stop.set()
    signal.signal(signal.SIGTERM, _on_signal)
    signal.signal(signal.SIGINT, _on_signal)

    job_q = queue.Queue()
    sched = threading.Lock()
    counts = {"busy": 0, "total": 0}

    def _spawn_worker():
        threading.Thread(target=_worker_loop, daemon=True).start()

    def _worker_loop():
        with sched:
            counts["total"] += 1
        released = False  # ensure this worker's slot is freed exactly once
        try:
            while not stop.is_set():
                try:
                    client_fd = job_q.get(timeout=0.1)
                except queue.Empty:
                    # Reap a surplus idle worker (always keep at least one) so a
                    # re-entrancy burst does not leave threads around forever.
                    with sched:
                        if counts["total"] > 1:
                            counts["total"] -= 1
                            released = True
                            # shm_tracker is __thread and its SHM is freed lazily
                            # on the NEXT dispatch; a reaped worker has none, so
                            # flush its last job's SHM now rather than leak it
                            # until pool shutdown.
                            morloc.shm_tracker_flush()
                            return
                    continue
                # Reserve a slot; if that saturates the pool, pre-spawn one so a
                # re-entrant callback always finds a free worker (no deadlock).
                with sched:
                    counts["busy"] += 1
                    saturated = counts["busy"] >= counts["total"]
                if saturated:
                    _spawn_worker()
                try:
                    run_job(client_fd)
                finally:
                    with sched:
                        counts["busy"] -= 1
        except BaseException as e:
            import traceback
            print(f"morloc pool worker thread fatal: {e!s}", file=sys.stderr)
            traceback.print_exc(file=sys.stderr)
            sys.stderr.flush()
        finally:
            # Release the slot on a FATAL exit too (the surplus reap above already
            # released it and set the flag). Leaking `total` would make the
            # saturation gate `busy >= total` stop tripping, so a re-entrant
            # callback could find no free worker and deadlock.
            if not released:
                with sched:
                    if counts["total"] > 0:
                        counts["total"] -= 1

    def _listener_loop():
        # Single acceptor: pull each client off the daemon and hand it to a
        # worker via the in-process queue (no fd-passing needed among threads).
        # A 200ms accept timeout lets this thread observe `stop` and exit so it
        # can be joined BEFORE close_daemon frees the daemon struct (otherwise it
        # would dereference freed memory on shutdown).
        while not stop.is_set():
            try:
                client_fd = morloc.wait_for_client(daemon, 200000)
            except Exception as e:
                print(f"In python daemon, failed to connect to client: {e!s}", file=sys.stderr)
                continue
            if client_fd and client_fd > 0:
                job_q.put(client_fd)

    _spawn_worker()
    listener = threading.Thread(target=_listener_loop, daemon=True)
    listener.start()

    try:
        while not stop.is_set():
            time.sleep(0.05)
    finally:
        stop.set()
        # Join the listener so it is no longer inside wait_for_client(daemon)
        # before we free the daemon struct. It polls `stop` every 200ms, so this
        # returns promptly; the bound keeps a wedged listener from hanging exit.
        listener.join(timeout=2.0)
        sys.stdout.flush()
        try:
            morloc.close_daemon(daemon)
        except Exception:
            pass


if __name__ == "__main__":
    # Line-buffer stderr so diagnostic output is not lost when pool is killed.
    # stdout is left fully buffered for performance (genome-scale piping) and
    # flushed explicitly after each job and during shutdown.
    sys.stderr.reconfigure(line_buffering=True)

    # Force the 'fork' multiprocessing start method on ALL platforms. Two
    # reasons, both requiring fork:
    #   1. The fork pool hands each worker the shared job-queue socketpair BY FD
    #      NUMBER and relies on inheritance; spawn (macOS default) gives a fresh
    #      interpreter with no inherited fds (socket.fromfd -> EBADF/ENOTSOCK).
    #   2. User multiprocessing.Pool must be able to run morloc manifolds, which
    #      are exec'd into this process's globals (not an importable module).
    #      Under spawn the mp worker re-imports and cannot find them
    #      ("name 'foo' is not defined"); under fork it inherits them. This is
    #      the user-parallel-map capability, which must work on macOS too.
    # Forking this (thread-model) pool process is safe on macOS in practice: the
    # nexus exports OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES and the pool's own
    # threads are plain socket-I/O pthreads holding no Apple-framework locks at
    # fork time. On Linux fork is already the default (no-op).
    import multiprocessing as _mp
    try:
        _mp.set_start_method("fork", force=True)
    except (RuntimeError, ValueError):
        pass

    # Request SIGTERM when the parent process (nexus) dies (Linux only).
    # Without this, SIGKILL on the nexus leaves pool processes orphaned and
    # their SHM segments leak. macOS has no prctl/PR_SET_PDEATHSIG equivalent,
    # so this is a known parity gap there: a SIGKILL'd nexus can orphan pools
    # (normal shutdown is still clean via the nexus's process-group teardown).
    try:
        import ctypes
        _PR_SET_PDEATHSIG = 1
        ctypes.CDLL("libc.so.6", use_errno=True).prctl(_PR_SET_PDEATHSIG, signal.SIGTERM)
    except Exception:
        pass  # non-Linux (e.g. macOS): no PDEATHSIG -- see the note above

    # RawValue (no lock), not Value: the SIGTERM/SIGINT handler writes this flag,
    # and a Value's semaphore lock is not async-signal-safe -- acquiring it in the
    # handler while the interrupted code already holds it can deadlock/corrupt at
    # teardown. A single-byte flag needs no lock (byte writes are atomic).
    shutdown_flag = RawValue('b', False)  # Shared flag (lock-free)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Health check: confirm imports loaded and print version
    if len(sys.argv) > 1 and sys.argv[1] == "--health":
        # Actually load the user sources so --health still validates that the
        # program's imports resolve (they no longer run at module import).
        _mlc_load_user_sources()
        if _mlc_source_error is not None:
            sys.stdout.write('{"status":"error","version":"__MORLOC_VERSION__"}\n')
            print(_mlc_source_error, file=sys.stderr)
            sys.exit(1)
        sys.stdout.write('{"status":"ok","version":"__MORLOC_VERSION__"}\n')
        sys.exit(0)

    # Process arguments passed from the nexus
    try:
        socket_path = sys.argv[1]
        tmpdir = sys.argv[2]
        shm_basename = sys.argv[3]
    except IndexError:
        print("Usage: script.py <socket_path> <tmpdir> <shm_basename>")
        sys.exit(1)

    global_state["tmpdir"] = tmpdir

    # Thread-based pool (macOS default; forced anywhere via MORLOC_PY_POOL=thread).
    # Avoids forking a live CPython interpreter. Everything below this branch is
    # the fork-based process pool (Linux default / MORLOC_PY_POOL=fork).
    if _select_pool_mode() == "thread":
        run_thread_pool(socket_path, tmpdir, shm_basename)
        sys.exit(0)

    # Shared job queue: listener writes fds to write_sock, workers read from read_sock.
    # Only idle workers (blocked in recvmsg) pick up jobs, preventing the round-robin
    # deadlock where a callback gets dispatched to a busy worker.
    read_sock, write_sock = _socket.socketpair(_socket.AF_UNIX, _socket.SOCK_STREAM)

    num_workers = 1
    workers = []

    # Shared counters for dynamic worker spawning.
    # Workers increment busy_count before foreign_call and decrement after.
    # When all workers are busy, main process spawns a new one.
    busy_count = RawValue(ctypes.c_int, 0)
    total_workers = RawValue(ctypes.c_int, num_workers)
    wakeup_r, wakeup_w = os.pipe()
    os.set_blocking(wakeup_r, False)
    _shutdown_wakeup_fd = wakeup_w

    # Keep a dup of the read end so we can spawn new workers later
    spare_read_fd = os.dup(read_sock.fileno())

    for i in range(num_workers):
        worker = Process(target=worker_process,
                         args=(read_sock.fileno(), tmpdir, shm_basename, shutdown_flag,
                               busy_count, total_workers, wakeup_w))
        worker.start()
        workers.append(worker)
    read_sock.close()  # main/listener don't need the read end (spare_read_fd kept)

    # Start client listener process
    listener_process = Process(
        target=client_listener,
        args=(write_sock.fileno(), socket_path, tmpdir, shm_basename, shutdown_flag)
    )
    listener_process.start()
    write_sock.close()  # main doesn't need the write end

    # Main loop: monitor wake-up pipe, spawn new workers when all are busy,
    # and reap idle workers that have exited. poll() (not select.select) avoids
    # the FD_SETSIZE=1024 ceiling.
    wakeup_poller = select.poll()
    wakeup_poller.register(wakeup_r, select.POLLIN)
    while not shutdown_flag.value:
        events = wakeup_poller.poll(10)  # milliseconds (was 0.01s)
        if events:
            try:
                os.read(wakeup_r, 4096)  # drain pipe
            except OSError:
                pass

        # Reap dead workers (idle timeout or error exit)
        alive = []
        for w in workers:
            if w.is_alive():
                alive.append(w)
            else:
                w.join(timeout=0)
                w.close()
        workers = alive
        total_workers.value = max(1, len(workers))

        # Spawn a new worker if all are busy (or all have exited)
        if len(workers) == 0 or busy_count.value >= total_workers.value:
            w = Process(target=worker_process,
                        args=(spare_read_fd, tmpdir, shm_basename, shutdown_flag,
                              busy_count, total_workers, wakeup_w))
            w.start()
            workers.append(w)
            total_workers.value = len(workers)

    # Shutdown sequence
    os.close(wakeup_r)
    os.close(wakeup_w)
    os.close(spare_read_fd)

    # 1. Stop listener first
    listener_process.terminate()
    listener_process.join(timeout=0.001)
    listener_process.kill()
    listener_process.join()  # Final blocking reap
    listener_process.close()

    # 2. Terminate workers with escalating force
    for p in workers:
        if p.is_alive():
            p.kill()
        p.join()  # Final blocking reap
        p.close()

    sys.exit(0)
