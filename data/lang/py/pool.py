import signal
import faulthandler
import sys
import select
import errno
import os # required for setting path to morloc dependencies
import time
import gc
import copy
import array
import struct
import mmap
import socket as _socket
from collections import OrderedDict
from multiprocessing import Process
import ctypes
import functools
import importlib
import importlib.util


# Global variables for clean signal handling
daemon = None
# The daemon pointer is also held here, and is taken out exactly once by
# whichever of the signal handler and the normal exit path reaches it first.
# `list.pop` is a single C call, so the interpreter cannot process a pending
# signal partway through it: a handler re-entering during cleanup finds the
# slot empty rather than a pointer that is already being freed. Making the
# hand-off atomic is what lets the handler leave signal dispositions alone --
# see 'signal_handler'.
_daemon_slot = []
workers = []
global_state = dict()
_shutdown_wakeup_fd = -1
# Read end of the nexus's lifeline pipe (see morloc_lifeline_adopt), or -1.
_lifeline_fd = -1


def _fork_single_threaded(started):
    """FORK-6: a worker is forked only from a process with no other thread.
    Otherwise the pool ends at once, taking the processes it started."""
    n = morloc.thread_count()
    if n == 1:
        return
    sys.stderr.write(f"morloc pool: refusing to fork a worker from a process with {n} threads\n")
    sys.stderr.flush()
    for p in started:
        try:
            p.kill()
            p.join()
        except Exception:
            pass
    os._exit(1)


def _lifeline_ended():
    """After a poll reported the lifeline readable: whether the nexus is gone.
    Nothing is ever written to it, so readable means end of file."""
    try:
        return os.read(_lifeline_fd, 1) == b""
    except BlockingIOError:
        return False
    except OSError:
        return True

# The language preamble (runtime bootstrap: `import pymorloc as morloc`, path
# setup) and the generated schema/closure tables run at module top, in the
# coordinator parent -- they are thread-free and the parent depends on them.
# A wire codec is either a schema string (a value) or a closure codec: the
# closure tuple's schema, the codecs of the closure's arguments, and the
# codec of its result. A function whose result is a suspension (a closure
# of no arguments) has a closure codec as its result codec, and so on
# through any depth. Defined ahead of the generated closure table, which
# is built from it.
def mlc_closure_codec(tuple_schema, arg_codecs, res_codec):
    return ("__mlc_closure__", tuple_schema, arg_codecs, res_codec)

# The schema of every closure's wire tuple (home, manifold id, captured
# packets); must equal the one Morloc.CodeGenerator.Serial emits.
_MLC_CLOSURE_SCHEMA = "t3sjaau1"

# AUTO include preamble start
# <<<BREAK>>>
# AUTO include preamble end

# User `source` imports, by contrast, are deferred out of the parent and run in
# each worker AFTER fork (see _mlc_load_user_sources). Importing heavy libraries
# in the parent and then forking is unsafe on macOS: e.g. numpy loads Apple's
# Accelerate/libdispatch, which spins background threads, and forking a
# multithreaded process then deadlocks/aborts in the child (fork-after-threads).
# Deferring keeps user-library threads out of the parent (the runtime's own
# threads are fork-aware), and each worker initializes its libraries in its own
# process -- preserving full per-worker thread parallelism.
# The block is held verbatim in a raw string and exec'd into module globals (so
# the imports land at module scope exactly as if run at import time, past fork).
_mlc_user_sources = r'''
# AUTO include user-sources start
# <<<BREAK>>>
# AUTO include user-sources end
'''

_mlc_sources_loaded = False
_mlc_source_error = None

def _mlc_import_resolves_under(top, root):
    # True when a plain `import top` from user code reaches the file or package
    # directory under root. The standard machinery and the source-root search
    # then agree on the file, so the module can carry its real name.
    try:
        spec = importlib.util.find_spec(top)
    except (ImportError, ValueError):
        return False
    if spec is None:
        return False
    want = os.path.realpath(os.path.join(root, top))
    if spec.submodule_search_locations:
        return any(os.path.realpath(p) == want for p in spec.submodule_search_locations)
    return bool(spec.origin) and os.path.realpath(spec.origin) == want + ".py"


def _mlc_import_source(module_path):
    # Load a `source`d file by location rather than by module name.
    #
    # A user file may be named after a module that is already in sys.modules --
    # `copy` and `time` are imported above, and they pull in others such as
    # `heapq` transitively -- and importing by name returns that module instead
    # of the user's file, so none of their functions are found. The set is not
    # one a user can be expected to know, so resolve the file under the source
    # roots (the program's own, the working directory, the morloc module plane)
    # and load it from there.
    #
    # Whenever an ordinary import of the path's top-level name would land on
    # the same tree, the file is imported under its real dotted name through
    # the standard machinery: parent packages get their `__init__.py` and
    # `__path__`, relative imports inside the file work, and user code that
    # later says `from app.main import x` gets this module rather than a
    # second copy with its own globals.
    #
    # Otherwise the name belongs to something else -- the standard library, an
    # installed package, the runtime -- and the module is registered under a
    # reserved key, so it neither reads nor replaces that module: a plain
    # `import copy` from inside a user file still reaches the standard library.
    #
    # A path that has no dotted name (one reaching outside the program's tree
    # through `..`, or an absolute one) arrives as the file path itself and is
    # always loaded by location.
    if module_path.endswith(".py"):
        for root in _mlc_source_roots:
            candidate = os.path.normpath(os.path.join(root, module_path))
            if os.path.isfile(candidate):
                return _mlc_load_source_file(module_path, candidate)
        raise ImportError("sourced file not found: " + module_path)
    rel = module_path.replace(".", os.sep) + ".py"
    for root in _mlc_source_roots:
        candidate = os.path.join(root, rel)
        if os.path.isfile(candidate):
            if _mlc_import_resolves_under(module_path.split(".")[0], root):
                module = importlib.import_module(module_path)
                found = getattr(module, "__file__", None)
                if found and os.path.realpath(found) == os.path.realpath(candidate):
                    return module
            return _mlc_load_source_file(module_path, candidate)
    # Nothing on the search path: an installed package, imported by name.
    return importlib.import_module(module_path)


def _mlc_load_source_file(module_path, candidate):
    key = "_mlc_src_" + "".join(c if c.isalnum() else "_" for c in module_path)
    spec = importlib.util.spec_from_file_location(key, candidate)
    module = importlib.util.module_from_spec(spec)
    sys.modules[key] = module
    spec.loader.exec_module(module)
    return module


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
    # A channel read or write waits on another worker, which may need a
    # free worker of this pool to make progress.
    morloc.mlc_next = _tracked_on_channel(morloc.mlc_next, 1)
    morloc.mlc_write = _tracked_on_channel(morloc.mlc_write, 3)
    morloc.mlc_flush = _tracked_on_channel(morloc.mlc_flush, 0)
    morloc.mlc_close = _tracked_on_channel(morloc.mlc_close, 0)

def _tracked_on_channel(f, handle_pos):
    def g(*args):
        if morloc.mlc_is_channel(args[handle_pos]):
            return _tracked_call(f, *args)
        return f(*args)
    return g

def _tracked_foreign_call(*args):
    return _tracked_call(_original_foreign_call, *args)

def _tracked_call(f, *args):
    busy_addr = ctypes.addressof(_busy_ref)
    busy = morloc.atomic_add_int(busy_addr, 1)
    if busy >= _total_ref.value and _wakeup_fd >= 0:
        try:
            os.write(_wakeup_fd, b'!')
        except OSError:
            pass
    try:
        return f(*args)
    finally:
        morloc.atomic_add_int(busy_addr, -1)

def __mlc_wrap_log(group, start_tmpl, pass_tmpl, fail_tmpl, bench_key, fn):
    # bench_key is "group\tname\tlang" when the label carries
    # `benchmark: true`, else None. Only the success path records: a call that
    # raised did not do the work being measured.
    def go(*args):
        call_id = morloc.log_next_id()
        t0 = time.monotonic()
        if start_tmpl is not None:
            morloc.log_emit(start_tmpl, group, 0.0, call_id)
        try:
            r = fn(*args)
            dt = time.monotonic() - t0
            if pass_tmpl is not None:
                morloc.log_emit(pass_tmpl, group, dt, call_id)
            if bench_key is not None:
                morloc.bench_record(bench_key, dt)
            return r
        except BaseException:
            if fail_tmpl is not None:
                morloc.log_emit(fail_tmpl, group, time.monotonic() - t0, call_id)
            raise
    # keep the manifold's name: a closure over it is identified by it
    go.__name__ = getattr(fn, "__name__", "go")
    return go


# Defunctionalized-closure support. A morloc function value is a
# functools.partial over a manifold function m<mid>; when it crosses a language
# boundary it travels as the wire tuple (home_language, manifold_id,
# captured_packets) and is applied on the far side by calling back to this pool.

MLC_HOME_LANG = "py"


def _mlc_is_closure_codec(codec):
    return isinstance(codec, tuple) and len(codec) == 4 and codec[0] == "__mlc_closure__"


def mlc_encode(value, codec, self_contained=False):
    # Serialize a native value by its codec: a closure is reified first. A
    # self-contained packet embeds the value rather than referencing a
    # shared-memory block, for a value that must outlive this dispatch.
    if _mlc_is_closure_codec(codec):
        return morloc.put_value(mlc_reify(value, MLC_HOME_LANG), codec[1], self_contained)
    return morloc.put_value(value, codec, self_contained)


def mlc_decode(pkt, codec):
    # Deserialize a packet by its codec: a closure is reflected into a
    # callable that calls back to its home pool.
    if _mlc_is_closure_codec(codec):
        return mlc_reflect_from_tuple(morloc.get_value(pkt, codec[1]), codec[2], codec[3])
    return morloc.get_value(pkt, codec)


# A table arrives as a pyarrow.RecordBatch over shared memory. When the
# module maps Table to another library's type, convert here; the known
# libraries take the batch through the Arrow PyCapsule interface without a
# copy where they can. Any other name is called as module.attr(batch).
def mlc_table_import(batch, typename):
    if typename in ("arrow", "pyarrow.RecordBatch"):
        return batch
    if typename == "pyarrow.Table":
        import pyarrow
        return pyarrow.Table.from_batches([batch])
    if typename == "polars.DataFrame":
        import polars
        return polars.from_arrow(batch)
    if typename == "pandas.DataFrame":
        return batch.to_pandas()
    if typename == "duckdb.DuckDBPyRelation":
        import duckdb
        return duckdb.from_arrow(batch)
    modname, _, attr = typename.rpartition(".")
    if not modname:
        raise TypeError("cannot import a table as %r: not a dotted type name" % typename)
    return getattr(importlib.import_module(modname), attr)(batch)


def mlc_reify(f, home_lang):
    # Recover (home, mid, captured) from a closure and serialize its captured
    # values. A closure built here is a functools.partial over a manifold
    # function m<mid>, with the captured values in the manifold's
    # context-argument order; mlc_closure_table[mid] holds their codecs. A
    # closure reflected from another pool carries its origin and is passed
    # back as it came.
    origin = getattr(f, "__mlc_origin__", None)
    if origin is not None:
        return origin
    if isinstance(f, functools.partial):
        mid = _mlc_manifold_id(f.func)
        captured = list(f.args)
    else:
        # A manifold function itself: a function value with nothing captured.
        mid = _mlc_manifold_id(f)
        captured = []
    # The codecs say what this manifold captures. A missing entry, or a
    # count that disagrees, would otherwise serialize the wrong captures
    # (a zip truncates silently), so the far side would call back with
    # arguments the closure never had.
    if mid not in mlc_closure_table:
        raise RuntimeError(
            f"morloc: no closure codecs for manifold {mid}; it was not "
            "registered as a crossing closure")
    cap_codecs, bnd_codecs = mlc_closure_table[mid]
    # A partial application of the closure carries its applied arguments
    # after its context ('mlc_papply'), so the captured values are the
    # context and a prefix of the bound arguments.
    if not (len(cap_codecs) <= len(captured) <= len(cap_codecs) + len(bnd_codecs)):
        raise RuntimeError(
            f"morloc: manifold {mid} captured {len(captured)} values but "
            f"{len(cap_codecs)} context codecs are registered")
    cap_codecs = (cap_codecs + bnd_codecs)[:len(captured)]
    # A captured value is applied back after this dispatch has released
    # its blocks, so it must travel inside its packet.
    packets = [mlc_encode(c, s, True) for c, s in zip(captured, cap_codecs)]
    return (home_lang, mid, packets)


def mlc_spawn(f, handle, schema):
    # Start the producer `f` on the channel as a separate dispatch in its
    # home pool, without waiting for it. The channel travels as the
    # producer's OStream argument, after its captured values.
    home, mid, packets = mlc_reify(f, MLC_HOME_LANG)
    packets = list(packets) + [morloc.put_value(handle, schema, True)]
    sock = os.path.join(global_state["tmpdir"], "pipe-" + home)
    morloc.mlc_spawn(sock, mid, packets, handle)


def _mlc_manifold_id(f):
    # A morloc-made callable is a generated manifold named m<mid>. Anything
    # else -- a callable the host created, or one morloc received and did
    # not record an origin for -- cannot be reified: the far side has no
    # manifold to call back into. Mirrors the C++ pool's
    # "cannot reify a non-morloc closure".
    name = getattr(f, "__name__", None)
    if name is not None and name.startswith("m") and name[1:].isdigit():
        return int(name[1:])
    raise RuntimeError(
        "morloc: cannot reify a callable that morloc did not create "
        f"({name!r}); a function value made by host code has no identity "
        "to send across a pool boundary")


def _mlc_call_released(sock, mid, packets, own, res_codec):
    # Call a manifold in another pool and read its result. The packets in
    # `own` were made for this call alone, and the result is read into a value
    # of this pool's own, so each is released here rather than held until the
    # dispatch ends. A None codec reads a closure wire tuple.
    try:
        res = morloc.foreign_call(sock, mid, packets)
    finally:
        for p in own:
            morloc.release_packet_shm(p)
    try:
        if res_codec is None:
            return morloc.get_value(res, _MLC_CLOSURE_SCHEMA)
        return mlc_decode(res, res_codec)
    finally:
        morloc.release_packet_shm(res)


class _MlcOwnedPacket(bytes):
    # A packet made for one call's argument list. CPython drops it as soon as
    # the call has returned and the argument list is gone, and dropping it
    # releases the shared memory it names.
    __slots__ = ()

    def __del__(self):
        try:
            morloc.release_packet_shm(self)
        except Exception:
            pass


def _mlc_owned(packet):
    return _MlcOwnedPacket(packet)


def mlc_reflect_from_tuple(tup, arg_codecs, res_codec):
    # Rebuild a callable from an already-deserialized closure wire tuple
    # (home_lang, mid, captured_packets). On application it serializes its
    # arguments, appends them to the captured packets, calls back to the
    # producing pool via foreign_call on the closure's manifold id, and
    # decodes the result. Used when the closure is nested in an aggregate
    # whose enclosing get_value has already parsed the tuple.
    home_lang, mid, captured = tup
    sock = os.path.join(global_state["tmpdir"], "pipe-" + home_lang)
    def _call(*args):
        own = [mlc_encode(a, s) for a, s in zip(args, arg_codecs)]
        return _mlc_call_released(sock, mid, list(captured) + own, own, res_codec)
    _call.__mlc_origin__ = (home_lang, mid, list(captured))
    _call.__mlc_codecs__ = (arg_codecs, res_codec)
    return _call


def mlc_reflect(pkt, tuple_schema, arg_codecs, res_codec):
    # Rebuild a callable from a raw incoming closure wire packet: deserialize the
    # tuple, then reflect it. Used when the closure is the top-level crossing
    # value (the whole packet is the closure tuple).
    return mlc_reflect_from_tuple(morloc.get_value(pkt, tuple_schema), arg_codecs, res_codec)


def mlc_papply(f, xs):
    # Apply a function value to its first arguments. A staged function (one
    # that does work after these arguments and before the rest) runs that
    # work now, once, through its stage entry, and the closure it returns is
    # the result; otherwise the arguments are kept with the function, which
    # runs when it gets the rest. mlc_stage_table maps a flat entry's
    # manifold id to (context size, first stage point, stage entry id).
    xs = list(xs)
    if not xs:
        return f
    origin = getattr(f, "__mlc_origin__", None)
    if origin is not None:
        return _mlc_papply_remote(f, origin, xs)
    if isinstance(f, functools.partial):
        fn = f.func
        caps = list(f.args)
    else:
        fn = f
        caps = []
    name = getattr(fn, "__name__", None)
    if not (name is not None and name.startswith("m") and name[1:].isdigit()):
        # a function host code made: it has no stages morloc knows of
        return functools.partial(f, *xs)
    mid = int(name[1:])
    take, smid = _mlc_stage_take(mid, len(caps))
    if take is None or len(xs) < take:
        return functools.partial(fn, *caps, *xs)
    g = globals()["m" + str(smid)](*caps, *xs[:take])
    return mlc_papply(g, xs[take:])


def _mlc_stage_take(mid, ncaptured):
    # How many more arguments closure `mid`, holding `ncaptured` captured
    # values, takes before its stage point, and its stage entry; (None, None)
    # when it has none.
    entry = mlc_stage_table.get(mid)
    if entry is None:
        return None, None
    nctx, k, smid = entry
    return k - (ncaptured - nctx), smid


def _mlc_papply_remote(f, origin, xs):
    # The same, for a closure that lives in another pool: its stage entry
    # runs there, and the closure it returns comes back as a reflected proxy.
    home_lang, mid, packets = origin
    arg_codecs, res_codec = f.__mlc_codecs__
    take, smid = _mlc_stage_take(mid, len(packets))
    if take is None or len(xs) < take:
        more = [mlc_encode(x, c, True) for x, c in zip(xs, arg_codecs)]
        return mlc_reflect_from_tuple(
            (home_lang, mid, list(packets) + more), arg_codecs[len(xs):], res_codec)
    sock = os.path.join(global_state["tmpdir"], "pipe-" + home_lang)
    own = [mlc_encode(x, c) for x, c in zip(xs[:take], arg_codecs)]
    tup = _mlc_call_released(sock, smid, list(packets) + own, own, None)
    g = mlc_reflect_from_tuple(tup, arg_codecs[take:], res_codec)
    return mlc_papply(g, xs[take:])


def mlc_make_closure_dispatch(mid, arg_codecs, res_codec):
    # Serial dispatch wrapper for a closure manifold: decode the incoming
    # captured ++ bound argument packets, call the native manifold, and
    # encode the result. Registered under the closure's mid so a foreign
    # apply reaches it.
    fn = globals()["m" + str(mid)]
    def _wrapper(*sargs):
        args = [mlc_decode(s, sch) for s, sch in zip(sargs, arg_codecs)]
        return mlc_encode(fn(*args), res_codec)
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
    # Once a reply has started to go out, a failure must not be followed by
    # a second packet on the same connection: a reader would take its bytes
    # as the rest of the first.
    replying = False
    try:
        # Anything left on this thread outside a dispatch; a dispatch's own
        # entries are released as its reply is sent (send_reply).
        morloc.shm_tracker_flush()
        morloc.debug_flush_dispatch()
        client_data = morloc.stream_from_client(client_fd)

        # If deferred source loading failed, every call fails with that error
        # (the program cannot run without its imports). Report it as a clean
        # fail packet rather than a cryptic dispatch error.
        if _mlc_source_error is not None and not morloc.is_ping(client_data):
            sys.stdout.flush()
            replying = True
            morloc.send_reply(client_fd, morloc.make_fail_packet(_mlc_source_error))
            return

        is_local = morloc.is_local_call(client_data)
        if is_local or morloc.is_remote_call(client_data):
            table = dispatch if is_local else remote_dispatch
            (mid, args) = morloc.read_morloc_call_packet(client_data)

            try:
                result = table[mid](*args)
            except morloc.MorlocPipeClosed as e:
                result = morloc.make_pipe_closed_packet(str(e))
            except Exception as e:
                result = morloc.make_fail_packet(_with_debug_trace(str(e)))
            # A process forked by user code during this call exits as soon
            # as the call returns in it (FORK-12).
            morloc.exit_if_forked()

        elif(morloc.is_ping(client_data)):
            # The nexus abandons a readiness-ping connection when its probe
            # times out (common for slow-starting pools like Python) and
            # retries on a fresh connection. Answering the orphaned ping then
            # fails with a broken pipe -- benign, so send the pong silently and
            # do not route a failure through the loud "job failed" handler (the
            # C++/Rust pools already ignore pong-send failures the same way).
            sys.stdout.flush()
            try:
                morloc.send_reply(client_fd, morloc.pong(client_data))
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
        # SLOT-16: output the call left on @stdout is written before its reply.
        morloc.reclaim_stdio_after_dispatch()

        replying = True
        morloc.send_reply(client_fd, result)

    except Exception as e:
        # Try to send a fail packet back to the caller before giving up,
        # unless a reply already started to go out.
        if not replying:
            try:
                morloc.reclaim_stdio_after_dispatch()
                result = morloc.make_fail_packet(str(e))
                morloc.send_reply(client_fd, result)
            except Exception:
                pass
        print(f"job failed: {e!s}", file=sys.stderr)
    finally:
        morloc.exit_if_forked()
        # Reclaim any stdio singleton claim this dispatch left open (e.g. a
        # handler that raised past @close on a broken pipe). Runs on the
        # same worker thread that opened it, so the reclaim's call_id gate
        # matches. Without this a leaked @stdout claim wedges every later
        # open with "@stdout already open in this nexus".
        morloc.reclaim_stdio_after_dispatch()
        # send_reply has released what the dispatch held; this covers a
        # dispatch that ended without sending one.
        morloc.shm_tracker_flush()
        # Safety-net flush for any output from error handling paths
        sys.stdout.flush()
        # close child copy
        morloc.close_socket(client_fd)


def _worker_idle_timeout():
    """Seconds a surplus fork worker stays idle before it retires."""
    try:
        return int(os.environ["MORLOC_POOL_IDLE_TIMEOUT_MS"].strip()) / 1000.0
    except (KeyError, ValueError):
        return 60.0

WORKER_IDLE_TIMEOUT = _worker_idle_timeout()

def worker_process(listen_fd, tmpdir, shm_basename, shutdown_flag, busy_count, total_workers, wakeup_w, retire_w):
    # FORK-12: a process forked from this one (by user code while its sources
    # load) has another generation and never accepts calls.
    born = morloc.fork_generation()
    # Reset signal handlers inherited from main. If user code inside run_job
    # calls multiprocessing.Pool (or anything else that forks and later
    # SIGTERMs its own children), those grandchildren would otherwise inherit
    # main's signal_handler and flip the shared shutdown_flag, causing main
    # to SIGKILL this worker mid-response. See the multiprocessing-py-1 bug.
    signal.signal(signal.SIGTERM, signal.SIG_DFL)
    signal.signal(signal.SIGINT, signal.SIG_DFL)
    # A fatal signal in this worker prints its Python frames, which name the
    # manifold (`mN`) that was executing; the parent reports the signal.
    faulthandler.enable()
    morloc.set_fallback_dir(tmpdir)
    morloc.shinit(shm_basename, 0xffff)
    # Load user sources HERE, post-fork, in the worker's own process (see the
    # _mlc_user_sources note). A failure is recorded, not raised: run_job turns
    # it into a fail packet so the caller gets the real import error.
    _mlc_load_user_sources()
    _init_worker_tracking(busy_count, total_workers, wakeup_w)
    listener = _socket.socket(fileno=listen_fd)
    listener.setblocking(False)
    me = os.getpid()
    last_activity = time.monotonic()
    busy_since_idle_check = False
    warned_blocked = False
    trace_retire = os.environ.get("MORLOC_PY_TRACE_RETIRE") == "1"
    last_trace = 0.0

    def trace(what):
        refs = morloc.held_references()
        sys.stderr.write(f"morloc py worker {me}: {what} idle={time.monotonic() - last_activity:.1f}s "
                         f"workers={total_workers.value} busy={busy_count.value} refs={refs}\n")
        sys.stderr.flush()

    if trace_retire:
        trace("started")
    try:
        # poll() (not select.select) avoids the FD_SETSIZE=1024 ceiling: a
        # job-queue fd >= 1024 makes select.select raise ValueError and kill the
        # worker. The Rust/C/R pools already use poll(2) for this reason.
        while not shutdown_flag.value:
            events = morloc.wait_readable(listen_fd, 10)
            if shutdown_flag.value:
                break
            if events:
                if morloc.fork_generation() != born:
                    os._exit(0)
                # Every idle worker polls the one listening socket; those that
                # lose the race to accept find nothing and poll again.
                try:
                    client_fd = morloc.accept_client(listen_fd)
                except OSError as e:
                    if e.errno in (errno.EMFILE, errno.ENFILE, errno.ENOBUFS, errno.ENOMEM):
                        time.sleep(0.05)
                        continue
                    raise
                if client_fd < 0:
                    continue
                run_job(client_fd)
                busy_since_idle_check = True
                if trace_retire:
                    trace("job done")
            else:
                # FORK-15: leases of user-forked children that are gone.
                morloc.reclaim_leases()
                # A clock read is a system call on some hosts, so the idle
                # time is measured only from polls that found nothing.
                if busy_since_idle_check:
                    busy_since_idle_check = False
                    last_activity = time.monotonic()
            if trace_retire and not events and time.monotonic() - last_trace >= 1.0:
                last_trace = time.monotonic()
                trace("idle check")
            if not events and total_workers.value > 1 and time.monotonic() - last_activity > WORKER_IDLE_TIMEOUT:
                # SHM-8: a worker retires only holding no shared memory, and
                # says so; any other exit ends the pool.
                gc.collect()
                morloc.shm_tracker_flush()
                blockers = morloc.retire_blockers()
                if blockers == 0:
                    if trace_retire:
                        trace("retiring")
                    morloc.remove_own_temps()
                    os.write(retire_w, struct.pack("i", me))
                    break
                if not warned_blocked:
                    warned_blocked = True
                    sys.stderr.write(f"morloc py worker {me}: idle but holding {blockers} "
                                     "shared-memory references; it cannot retire\n")
                    sys.stderr.flush()
                last_activity = time.monotonic()
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
        # This worker is a process of its own and is leaving; its deferred
        # releases have no next dispatch to perform them, and a process
        # exiting does not decrement a reference other processes can see.
        # The worker is idle or already failing here, so nothing it holds is
        # still in use by a dispatch of its own.
        try:
            morloc.shm_tracker_flush()
        except Exception:
            pass
        listener.close()


def _hold_daemon(d):
    _daemon_slot.append(d)
    return d


def _take_daemon():
    # None when someone else already took it.
    try:
        return _daemon_slot.pop()
    except IndexError:
        return None


def signal_handler(sig, frame):
    global daemon
    # Re-entrancy is safe here and needs no change to signal dispositions.
    # Python processes pending signals between bytecodes, including while a
    # handler is running, so a second SIGTERM can re-enter this function at any
    # point; every step below is idempotent, and the daemon pointer is taken
    # from a slot that yields it to exactly one caller. Setting SIGTERM to
    # SIG_IGN here instead would race with the delivery of that second signal,
    # which the interpreter reports by printing
    # "OSError: Signal 15 ignored due to race condition" to stderr.
    shutdown_flag.value = True
    if _shutdown_wakeup_fd >= 0:
        try:
            os.write(_shutdown_wakeup_fd, b'!')
        except OSError:
            pass
    daemon = None
    d = _take_daemon()
    if d is not None:
        morloc.close_daemon(d)


def _read_retired(fd):
    """The pids retired workers have written to `fd` so far."""
    pids = set()
    while True:
        try:
            data = os.read(fd, 4096)
        except BlockingIOError:
            return pids
        if not data:
            return pids
        pids.update(p for (p,) in struct.iter_unpack("i", data[: len(data) // 4 * 4]))


def _report_worker_death(w, shutting_down=False):
    # A worker that died by a signal is the only sign a caller gets of what
    # went wrong: the pool process itself lives on, so the nexus sees no
    # exit status, only a closed socket. Read before close(), after which
    # exitcode is unavailable. A SIGTERM is a request to stop, never a
    # crash (the nexus terminates the whole process group at shutdown, and
    # the worker may take it before this process does); a SIGKILL during
    # shutdown is that same teardown escalating.
    code = w.exitcode
    if code is None:
        return
    if code < 0:
        if -code == signal.SIGTERM or (shutting_down and -code == signal.SIGKILL):
            return
        print(f"morloc py pool: worker {w.pid} crashed with signal {-code}", file=sys.stderr)
    elif code != 0:
        print(f"morloc py pool: worker {w.pid} exited with status {code}", file=sys.stderr)
    sys.stderr.flush()


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
    # On Linux fork is already the default (no-op).
    import multiprocessing as _mp
    try:
        _mp.set_start_method("fork", force=True)
    except (RuntimeError, ValueError):
        pass

    # The nexus's lifeline: the main loops below poll it and end this pool's
    # process group when the nexus ends, however it ends.
    _lifeline_fd = morloc.lifeline_adopt()

    # Python pools fork their workers; there is no other model.
    if os.environ.get("MORLOC_PY_POOL", "").strip().lower() not in ("", "fork"):
        print(f"morloc py pool: MORLOC_PY_POOL={os.environ['MORLOC_PY_POOL']} is not supported; "
              "Python pools fork their workers", file=sys.stderr)
        sys.exit(1)

    # Shared with the workers, which inherit the mapping. Unlike a
    # multiprocessing value it needs no lock, which the signal handler could
    # not take safely, and leaves no pymp-* temporary file behind.
    _shared_words = mmap.mmap(-1, 16)
    shutdown_flag = ctypes.c_byte.from_buffer(_shared_words, 0)
    busy_count = ctypes.c_int.from_buffer(_shared_words, 4)
    total_workers = ctypes.c_int.from_buffer(_shared_words, 8)

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
    morloc.set_self_socket(socket_path)

    # The pool's listening socket, bound before any worker exists. Idle
    # workers accept on it themselves; a busy worker does not poll it, so a
    # call never lands on a worker waiting on another pool.
    daemon = _hold_daemon(morloc.start_daemon(socket_path, tmpdir, shm_basename, 0xffff))
    listen_fd = morloc.daemon_listen_fd(daemon)

    num_workers = 1
    workers = []

    # Workers count themselves busy across a foreign call; when every worker
    # is busy, this process starts another.
    total_workers.value = num_workers
    wakeup_r, wakeup_w = os.pipe()
    os.set_blocking(wakeup_r, False)
    _shutdown_wakeup_fd = wakeup_w

    # A worker that retires writes its pid here first (SHM-8).
    retire_r, retire_w = os.pipe()
    os.set_blocking(retire_r, False)
    retired = set()
    pool_failure = None

    for i in range(num_workers):
        worker = Process(target=worker_process,
                         args=(listen_fd, tmpdir, shm_basename, shutdown_flag,
                               busy_count, total_workers, wakeup_w, retire_w))
        _fork_single_threaded(workers)
        worker.start()
        workers.append(worker)

    # Main loop: monitor wake-up pipe, spawn new workers when all are busy,
    # and reap idle workers that have exited. poll() (not select.select) avoids
    # the FD_SETSIZE=1024 ceiling.
    wakeup_poller = select.poll()
    wakeup_poller.register(wakeup_r, select.POLLIN)
    if _lifeline_fd >= 0:
        wakeup_poller.register(_lifeline_fd, select.POLLIN)
    while not shutdown_flag.value:
        events = wakeup_poller.poll(10)  # milliseconds (was 0.01s)
        for fd, _ in events:
            if fd == _lifeline_fd:
                if _lifeline_ended():
                    wakeup_poller.unregister(_lifeline_fd)
                    morloc.lifeline_teardown()
                continue
            try:
                os.read(wakeup_r, 4096)  # drain pipe
            except OSError:
                pass

        # Reap ended workers. A retired worker wrote its pid before it
        # exited, so the pipe is read again once its exit is seen.
        alive = []
        for w in workers:
            if w.is_alive():
                alive.append(w)
                continue
            w.join(timeout=0)
            if w.pid not in retired:
                retired |= _read_retired(retire_r)
            clean = w.pid in retired
            retired.discard(w.pid)
            _report_worker_death(w, shutting_down=bool(shutdown_flag.value))
            if not clean and not shutdown_flag.value and pool_failure is None:
                pool_failure = f"worker {w.pid} ended without retiring"
            w.close()
        workers = alive
        total_workers.value = max(1, len(workers))
        if pool_failure is not None:
            # SHM-8: what the process held is recovered only by ending the
            # pool, so the nexus recovers its shared memory.
            print(f"morloc py pool: {pool_failure}; ending the pool so its shared memory is recovered",
                  file=sys.stderr)
            sys.stderr.flush()
            break

        # Spawn a new worker if all are busy (or all have exited)
        if not shutdown_flag.value and (len(workers) == 0 or busy_count.value >= total_workers.value):
            w = Process(target=worker_process,
                        args=(listen_fd, tmpdir, shm_basename, shutdown_flag,
                              busy_count, total_workers, wakeup_w, retire_w))
            _fork_single_threaded(workers)
            w.start()
            workers.append(w)
            total_workers.value = len(workers)

    # Shutdown sequence
    os.close(wakeup_r)
    os.close(wakeup_w)
    os.close(retire_r)
    os.close(retire_w)

    # 1. Stop taking calls: close the listening socket and remove its path,
    # since idle workers would accept until they are killed.
    d = _take_daemon()
    if d is not None:
        morloc.close_daemon(d)

    # 2. Terminate workers with escalating force. A worker that already died
    # by a signal is reported here too, since the reap loop above may not
    # have run again before shutdown.
    for p in workers:
        if p.is_alive():
            p.kill()
        else:
            _report_worker_death(p, shutting_down=True)
        p.join()  # Final blocking reap
        p.close()

    sys.exit(1 if pool_failure is not None else 0)
