import faulthandler
import sys
import os # required for setting path to morloc dependencies
import time
import gc
import copy
import array
import struct
import socket as _socket
from collections import OrderedDict
import functools
import importlib
import importlib.util


global_state = dict()

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
# functools.partial over a manifold function __mm_<mid>; when it crosses a language
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
    # function __mm_<mid>, with the captured values in the manifold's
    # context-argument order; mlc_closure_table[mid] holds their codecs. A
    # closure reflected from another pool carries its origin and is passed
    # back as it came.
    origin = getattr(f, "__mlc_origin__", None)
    if origin is not None:
        return origin
    if isinstance(f, functools.partial) and not f.keywords:
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
    if mid not in globals().get("mlc_closure_table", {}):
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


_MLC_MANIFOLD_PREFIX = "__mm_"


def _mlc_mid_of(f):
    # The id of the generated manifold that f is, or None. The name alone
    # does not decide it: a host function may share the name.
    name = getattr(f, "__name__", None)
    if name is None or not name.startswith(_MLC_MANIFOLD_PREFIX):
        return None
    digits = name[len(_MLC_MANIFOLD_PREFIX):]
    if digits.isdigit() and globals().get(name) is f:
        return int(digits)
    return None


def _mlc_manifold_id(f):
    # Anything but a generated manifold -- a callable the host created, or
    # one morloc received and did not record an origin for -- cannot be
    # reified: the far side has no manifold to call back into. Mirrors the
    # C++ pool's "cannot reify a non-morloc closure".
    mid = _mlc_mid_of(f)
    if mid is not None:
        return mid
    name = getattr(f, "__name__", None)
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
            return morloc.get_value(res, mlc_closure_schema)
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
    # A [U8] decodes as a list (NATIVE-7); foreign_call takes packets as bytes.
    captured = [bytes(p) for p in captured]
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
    if isinstance(f, functools.partial) and not f.keywords:
        fn = f.func
        caps = list(f.args)
    else:
        fn = f
        caps = []
    mid = _mlc_mid_of(fn)
    if mid is None:
        # a function host code made: it has no stages morloc knows of
        return functools.partial(f, *xs)
    take, smid = _mlc_stage_take(mid, len(caps))
    if take is None or len(xs) < take:
        return functools.partial(fn, *caps, *xs)
    g = globals()[_MLC_MANIFOLD_PREFIX + str(smid)](*caps, *xs[:take])
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
    fn = globals()[_MLC_MANIFOLD_PREFIX + str(mid)]
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

    # Python pools fork their workers; there is no other model.
    if os.environ.get("MORLOC_PY_POOL", "").strip().lower() not in ("", "fork"):
        print(f"morloc py pool: MORLOC_PY_POOL={os.environ['MORLOC_PY_POOL']} is not supported; "
              "Python pools fork their workers", file=sys.stderr)
        sys.exit(1)

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

    def _worker_init():
        # A fatal signal in a worker prints its Python frames, which name the
        # manifold (`mN`) that was executing; the coordinator reports the signal.
        faulthandler.enable()
        morloc.set_fallback_dir(tmpdir)
        morloc.shinit(shm_basename, 0xffff)
        # User sources load here, in the worker: a failure is recorded and
        # returned per call by run_job.
        _mlc_load_user_sources()

    def _before_retire():
        # SHM-8: release what Python still holds before the count is checked.
        gc.collect()
        morloc.shm_tracker_flush()

    def _worker_exit(status):
        morloc.shm_tracker_flush()
        sys.stdout.flush()
        sys.stderr.flush()

    # The scheduler (libmorloc) binds the socket, forks and retires workers,
    # and returns the pool's exit status; workers never return from it.
    sys.exit(morloc.fork_pool_run(socket_path, tmpdir, shm_basename,
                                  _worker_init, run_job, _before_retire, _worker_exit))
