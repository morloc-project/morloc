# The preamble (dyn.load of librmorloc.so, library(bit64), .morloc.source
# definition) and the generated schema/closure tables run at module top, in the
# coordinator parent -- they are thread-free and the parent's .Call wrappers and
# fork machinery depend on them (dyn.load in particular MUST run before the
# parent calls any morloc_* function).

# Every name the pool defines lives in .__morloc, the one global name it
# reserves besides the manifolds' .__mm_<id>, so a user source can neither
# replace one nor be shadowed by one (.morloc.source, .morloc.pooldir and
# .morloc.libdir are also bound globally, for user files). Its parent is the
# base environment: pool code finds base R and its own names, never a
# user's. Generated manifolds are global, where R compiles them, so their
# calls of sourced functions resolve as in user code; they refer to pool
# names through .__morloc, which is bound inside the environment too. A
# source that defines, replaces or removes a reserved name fails to load.
.__morloc <- new.env(parent = baseenv())
assign(".__morloc", .__morloc, envir = .__morloc)
lockBinding(".__morloc", globalenv())

local({

# A wire codec is either a schema string (a value) or a closure codec: the
# closure tuple's schema, the codecs of the closure's arguments, and the
# codec of its result, recursively. Defined ahead of the generated closure
# table, which is built from it.
mlc_closure_codec <- function(tuple_schema, arg_codecs, res_codec) {
  structure(list(tuple_schema = tuple_schema, arg_codecs = arg_codecs, res_codec = res_codec),
            class = "mlc_closure_codec")
}

mlc_closure_table <- list()

# A generated manifold's function, by id.
mlc_manifold <- function(mid) {
  get(paste0(MLC_MANIFOLD_PREFIX, as.character(as.integer(mid))), envir = globalenv(), inherits = FALSE)
}

# The global names the pool reserves: the manifolds and .__morloc itself.
# Their bindings are locked, so a source can only add or remove one.
mlc_reserved_globals <- function() {
  manifolds <- paste0("^", gsub(".", "\\.", MLC_MANIFOLD_PREFIX, fixed = TRUE), "[0-9]")
  names <- ls(globalenv(), all.names = TRUE)
  names[grepl(manifolds, names) | names == ".__morloc"]
}

# AUTO include preamble start
# <<<BREAK>>>
# AUTO include preamble end

# User `source` calls, by contrast, are deferred out of the parent and run in
# each worker AFTER fork (see mlc_load_user_sources). Loading a package that
# spins threads (an OpenMP / Accelerate-BLAS user library) in the parent and then
# forking is unsafe on macOS (fork-after-threads deadlock/abort). Deferring keeps
# user-library threads out of the parent; each worker loads its libraries in its own process,
# preserving per-worker thread parallelism. Held as an unevaluated function body
# and eval'd here; source() assigns into globalenv, so sourced symbols land
# at global scope exactly as if run at load time.
.mlc_sources_fn <- function() {
# AUTO include user-sources start
# <<<BREAK>>>
# AUTO include user-sources end
}

# The pool's mutable state. The bindings of .__morloc are locked; these
# environments are not.
mlc_state <- new.env(parent = emptyenv())
mlc_state$sources_loaded <- FALSE
mlc_state$source_error <- NULL

mlc_load_user_sources <- function() {
  if (mlc_state$sources_loaded) return(invisible())
  # Import-time stdout must not corrupt the data stream shared with the nexus.
  sink(stderr(), type = "output")
  on.exit(sink(NULL, type = "output"), add = TRUE)
  tryCatch({
    before <- mlc_reserved_globals()
    eval(body(.mlc_sources_fn), envir = .__morloc)
    after <- mlc_reserved_globals()
    changed <- union(setdiff(after, before), setdiff(before, after))
    if (length(changed) > 0) {
      stop(paste0("a source defines or removes a name the R pool reserves: ", paste(sort(changed), collapse = ", ")))
    }
    mlc_state$sources_loaded <- TRUE
  }, error = function(e) {
    mlc_state$source_error <- paste("failed to load pool sources:", .mlc_message(e))
    cat(mlc_state$source_error, "\n", file = stderr())
  })
  invisible()
}

morloc_foreign_call                  <- function(...){ .Call("morloc_foreign_call",                  ...) }
morloc_remote_call                   <- function(...){ .Call("morloc_remote_call",                   ...) }
# The third argument is codegen's per-call answer to "can this value carry an
# interior NUL", which R cannot represent. It defaults to TRUE for the call
# sites in this template, whose schemas are only known at run time (closure
# reflection, remote dispatch) -- checking is the safe answer there, and none
# of those paths is hot enough for the walk to matter. Generated manifolds
# always pass the flag explicitly.
morloc_get_value                     <- function(packet, schema, check_nul = TRUE){ .Call("morloc_get_value", packet, schema, check_nul) }
morloc_put_value                     <- function(value, schema, self_contained = FALSE){ .Call("morloc_put_value", value, schema, self_contained) }
morloc_release_packet_shm            <- function(...){ .Call("morloc_release_packet_shm",            ...) }
morloc_mlc_show                      <- function(...){ .Call("morloc_mlc_show",                      ...) }
morloc_mlc_save_voidstar             <- function(...){ .Call("morloc_mlc_save_voidstar",             ...) }
morloc_mlc_save_json                 <- function(...){ .Call("morloc_mlc_save_json",                 ...) }
morloc_mlc_load                      <- function(...){ .Call("morloc_mlc_load",                      ...) }
morloc_mlc_hash                      <- function(...){ .Call("morloc_mlc_hash",                      ...) }
morloc_mlc_compare                   <- function(...){ .Call("morloc_mlc_compare",                   ...) }
morloc_mlc_read                      <- function(...){ .Call("morloc_mlc_read",                      ...) }
morloc_mlc_conform                   <- function(...){ .Call("morloc_mlc_conform",                   ...) }
morloc_mlc_unpack                    <- function(schema, packet){ morloc_get_value(as.raw(packet), schema) }
morloc_mlc_open_ifile                <- function(...){ .Call("morloc_mlc_open_ifile",                ...) }
morloc_mlc_close                     <- function(...){ .Call("morloc_mlc_close",                     ...) }
morloc_mlc_tmpfile                   <- function(...){ .Call("morloc_mlc_tmpfile",                   ...) }
morloc_mlc_unlink_tmp                <- function(...){ .Call("morloc_mlc_unlink_tmp",                ...) }
morloc_mlc_tell                      <- function(...){ .Call("morloc_mlc_tell",                      ...) }
morloc_mlc_cell_new                  <- function(...){ .Call("morloc_mlc_cell_new",                  ...) }
morloc_mlc_cell_get                  <- function(...){ .Call("morloc_mlc_cell_get",                  ...) }
morloc_mlc_cell_put                  <- function(...){ .Call("morloc_mlc_cell_put",                  ...) }
morloc_mlc_cell_count                <- function(...){ .Call("morloc_mlc_cell_count",                ...) }
morloc_mlc_cell_slot                 <- function(...){ .Call("morloc_mlc_cell_slot",                 ...) }
morloc_mlc_cell_free                 <- function(...){ .Call("morloc_mlc_cell_free",                 ...) }
morloc_mlc_fschema                   <- function(...){ .Call("morloc_mlc_fschema",                   ...) }
morloc_mlc_ifile_walk                <- function(...){ .Call("morloc_mlc_ifile_walk",                ...) }
morloc_mlc_ifile_length              <- function(...){ .Call("morloc_mlc_ifile_length",              ...) }
morloc_mlc_next                      <- function(...){ .Call("morloc_mlc_next",                      ...) }
morloc_mlc_next_frame                <- function(...){ .Call("morloc_mlc_next_frame",                ...) }
morloc_mlc_stream_layout             <- function(...){ .Call("morloc_mlc_stream_layout",             ...) }
morloc_mlc_stream                    <- function(...){ .Call("morloc_mlc_stream",                    ...) }
morloc_mlc_open_ostream              <- function(...){ .Call("morloc_mlc_open_ostream",              ...) }
morloc_mlc_open_istream              <- function(...){ .Call("morloc_mlc_open_istream",              ...) }
morloc_mlc_open_stdout               <- function(...){ .Call("morloc_mlc_open_stdout",               ...) }
morloc_mlc_open_stdin                <- function(...){ .Call("morloc_mlc_open_stdin",                ...) }
morloc_mlc_open_stderr               <- function(...){ .Call("morloc_mlc_open_stderr",               ...) }
morloc_mlc_write                     <- function(...){ .Call("morloc_mlc_write",                     ...) }
morloc_mlc_append                    <- function(...){ .Call("morloc_mlc_append",                    ...) }
morloc_mlc_concat                    <- function(...){ .Call("morloc_mlc_concat",                    ...) }
morloc_mlc_flush                     <- function(...){ .Call("morloc_mlc_flush",                     ...) }
morloc_mlc_open_channel              <- function(...){ .Call("morloc_mlc_open_channel",              ...) }
morloc_mlc_settle                    <- function(...){ .Call("morloc_mlc_settle",                    ...) }
morloc_mlc_spawn                     <- function(...){ .Call("morloc_mlc_spawn",                     ...) }
# Every accumulator of a cell, in its wire form; with morloc_mlc_reduce_list
# it runs @cellreduce.
morloc_mlc_cell_slots <- function(schema, handle) {
  on.exit(try(morloc_mlc_cell_free(handle), silent = TRUE), add = TRUE)
  n <- morloc_mlc_cell_count(handle)
  if (n < 1) stop("@fold: the accumulator holds nothing to merge")
  lapply(seq_len(n) - 1, function(i) morloc_mlc_cell_slot(handle, i, schema))
}

# A non-empty list folded with `combine`.
morloc_mlc_reduce_list <- function(combine, xs) {
  Reduce(combine, xs)
}

# @replay: call `fn` on every frame (sub-packet) of the stream, in order,
# each as the list it holds. Only the end of the stream (NULL) stops it.
morloc_mlc_replay <- function(schema, handle, fn) {
  repeat {
    frame <- morloc_mlc_next_frame(schema, handle)
    if (is.null(frame)) break
    fn(frame)
  }
  NULL
}

# @throw: raise a classed condition.
morloc_mlc_throw <- function(msg) {
  stop(structure(
    class = c("MorlocException", "error", "condition"),
    list(message = msg, call = NULL)
  ))
}
# A genuine morloc-invariant violation (compiler bug, contract violation,
# unreachable branch) ends the pool (PANIC-5).
morloc_mlc_internal_abort <- function(msg) {
  cat(paste0("morloc internal error (R pool): ", msg, "\n"), file = stderr())
  morloc_exit(70L)
}
# The downstream reader of a stream closed it: the call ends. Not an
# "error" condition, so no error handler, @try's included, takes it.
morloc_mlc_pipe_closed <- function() {
  stop(structure(
    class = c("MorlocPipeClosed", "condition"),
    list(message = "@stdout: downstream pipe closed", call = NULL)
  ))
}
# @try body: run the thunk and convert the outcome to data. `ok` wraps the
# value, `err` the message; codegen supplies both because only it knows how
# this Try is represented in R. An interrupt is not an "error" condition in
# R, so tryCatch lets it past without help.
morloc_mlc_try <- function(body, ok, err) {
  n0 <- sys.nframe()
  trace <- NULL
  tryCatch(
    withCallingHandlers(ok(body()), error = function(e) {
      if (is.null(trace)) trace <<- .mlc_trace_below(n0)
    }),
    error = function(e) err(paste0(.mlc_message(e), trace))
  )
}

# A condition's message. Base's generic, dispatched from the global
# environment so a user's own condition classes keep their methods.
.mlc_message <- function(e) {
  tryCatch(
    eval(quote(base::conditionMessage(e)), list(e = e), globalenv()),
    error = function(failed) if (is.character(e$message)) e$message else "an error whose message cannot be read"
  )
}

# A failure's trace (DIAG-5, EMIT-3): each named generated function carries
# its label as its "morloc_frame" attribute, and the frames between the
# failure and frame `n0` are read from the stack when an error is signalled,
# innermost first. Nothing is done per call.
.mlc_trace_below <- function(n0) {
  n <- sys.nframe()
  if (n <= n0) return("")
  labels <- character(0)
  for (i in seq.int(n, n0 + 1L)) {
    label <- attr(sys.function(i), "morloc_frame", exact = TRUE)
    if (!is.null(label)) labels <- c(labels, label)
  }
  if (length(labels) == 0L) "" else paste0("\n  at ", labels, collapse = "")
}

# A call from the dispatch: the trace is appended to the message of a failure
# that escapes it.
.mlc_entry <- function(fn, ...) {
  n0 <- sys.nframe()
  trace <- NULL
  tryCatch(
    withCallingHandlers(fn(...), error = function(e) {
      if (is.null(trace)) trace <<- .mlc_trace_below(n0)
    }),
    error = function(e) stop(paste0(.mlc_message(e), trace), call. = FALSE)
  )
}
morloc_fork_pool_run_r               <- function(...){ .Call("morloc_fork_pool_run_r",               ...) }
morloc_set_line_buffered             <- function(...){ .Call("morloc_set_line_buffered",             ...) }
morloc_exit                          <- function(...){ .Call("morloc_exit",                          ...) }
morloc_cache_key_compute             <- function(...){ .Call("r_morloc_cache_key_compute",           ...) }
morloc_debug_record_frame            <- function(...){ .Call("r_morloc_debug_record_frame",          ...) }
morloc_debug_flush_dispatch          <- function(  ){ .Call("r_morloc_debug_flush_dispatch"        ) }
morloc_cache_lookup                  <- function(...){ .Call("r_morloc_cache_lookup",                ...) }
morloc_cache_store                   <- function(...){ .Call("r_morloc_cache_store",                 ...) }
morloc_cache_record_hit              <- function( ){ .Call("r_morloc_cache_record_hit"             ) }
morloc_cache_record_miss             <- function( ){ .Call("r_morloc_cache_record_miss"            ) }
morloc_cache_record_store            <- function( ){ .Call("r_morloc_cache_record_store"           ) }

global_state <- new.env(parent = emptyenv())

.mlc_wrap_log <- function(group, start_tmpl, pass_tmpl, fail_tmpl, bench_key, fn) {
  # Eagerly resolve `fn` so the closure captures the ORIGINAL function. The
  # rebinding pattern `.__mm_N <- .mlc_wrap_log(..., .__mm_N)` reassigns
  # the global .__mm_N to this wrapper; without force(), R's lazy promise for
  # `fn` only resolves when first used inside the wrapper, by which point
  # .__mm_N is the wrapper itself -- the wrapper calls itself, infinite
  # recursion.
  force(group); force(start_tmpl); force(pass_tmpl); force(fail_tmpl)
  force(bench_key); force(fn)
  function(...) {
    call_id <- .Call("r_morloc_log_next_id")
    t0 <- Sys.time()
    if (!is.null(start_tmpl)) {
      .Call("r_morloc_log_emit", start_tmpl, group, 0, call_id)
    }
    tryCatch({
      r <- fn(...)
      dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      if (!is.null(pass_tmpl)) {
        .Call("r_morloc_log_emit", pass_tmpl, group, dt, call_id)
      }
      if (!is.null(bench_key)) {
        .Call("r_morloc_bench_record", bench_key, dt)
      }
      r
    }, error = function(e) {
      if (!is.null(fail_tmpl)) {
        dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        .Call("r_morloc_log_emit", fail_tmpl, group, dt, call_id)
      }
      stop(e)
    })
  }
}

# Defunctionalized-closure support. An R closure produced by this pool carries
# its manifold id and captured values as attributes (attached at construction);
# when it crosses a language boundary it travels as the wire tuple
# (home_language, manifold_id, captured_packets) and is applied on the far side
# by calling back to this pool.

MLC_MANIFOLD_PREFIX <- ".__mm_"
MLC_HOME_LANG <- "r"

mlc_is_closure_codec <- function(codec) inherits(codec, "mlc_closure_codec")

# Serialize a native value by its codec: a closure is reified first.
# Serialize a native value by its codec: a closure is reified first. A
# self-contained packet embeds the value rather than referencing a
# shared-memory block, for a value that must outlive this dispatch.
mlc_encode <- function(value, codec, self_contained = FALSE) {
  if (mlc_is_closure_codec(codec)) {
    morloc_put_value(mlc_reify(value, MLC_HOME_LANG), codec$tuple_schema, self_contained)
  } else {
    morloc_put_value(value, codec, self_contained)
  }
}

# Deserialize a packet by its codec: a closure is reflected into a callable
# that calls back to its home pool.
mlc_decode <- function(pkt, codec) {
  if (mlc_is_closure_codec(codec)) {
    mlc_reflect_from_tuple(morloc_get_value(pkt, codec$tuple_schema), codec$arg_codecs, codec$res_codec)
  } else {
    morloc_get_value(pkt, codec)
  }
}

# Recover (home, mid, captured) from a closure and serialize its captured
# values; mlc_closure_table[[mid]] holds their codecs. A closure reflected
# from another pool carries its origin and is passed back as it came.
# A table arrives as an arrow RecordBatch. When the module maps Table to
# another type, convert here; the export side accepts anything
# arrow::as_record_batch understands, so a data.frame round-trips.
mlc_table_import <- function(batch, typename) {
  if (typename %in% c("arrow", "arrow::RecordBatch", "RecordBatch")) return(batch)
  if (typename == "arrow::Table") return(arrow::arrow_table(batch))
  if (typename == "data.frame") return(as.data.frame(batch))
  if (typename == "tibble") return(tibble::as_tibble(batch))
  stop(paste0("cannot import a table as '", typename, "'"))
}

mlc_reify <- function(f, home_lang) {
  origin <- attr(f, "morloc_origin")
  if (!is.null(origin)) return(origin)
  mid <- attr(f, "morloc_mid")
  if (is.null(mid)) {
    stop("morloc: cannot reify a function that morloc did not create; a function value made by host code has no identity to send across a pool boundary", call. = FALSE)
  }
  captured <- attr(f, "morloc_captured")
  if (is.null(captured)) captured <- list()
  entry <- mlc_closure_table[[as.character(as.integer(mid))]]
  # A partial application of the closure carries its applied arguments after
  # its context ('mlc_papply'), so the captured values are the context and a
  # prefix of the bound arguments.
  cap_codecs <- if (is.null(entry)) list() else c(entry[[1]], entry[[2]])
  # A captured value is applied back after this dispatch has released its
  # blocks, so it must travel inside its packet.
  packets <- lapply(seq_along(captured), function(i) mlc_encode(captured[[i]], cap_codecs[[i]], TRUE))
  list(home_lang, as.integer(mid), packets)
}

# Start the producer `f` on the channel as a separate dispatch in its home
# pool, without waiting for it. The channel travels as the producer's
# OStream argument, after its captured values.
mlc_spawn <- function(f, handle, schema) {
  r <- mlc_reify(f, MLC_HOME_LANG)
  packets <- c(r[[3]], list(morloc_put_value(handle, schema, TRUE)))
  sock <- paste0(global_state$tmpdir, "/pipe-", r[[1]])
  morloc_mlc_spawn(sock, as.integer(r[[2]]), packets, handle)
  invisible(NULL)
}

# Call a manifold in another pool and read its result. The packets in `own`
# were made for this call alone, and the result is read into a value of this
# pool's own, so each is released here, on error too, rather than held until
# the dispatch ends. A NULL codec reads a closure wire tuple.
mlc_call_released <- function(sock, mid, packets, own, res_codec) {
  res <- NULL
  on.exit({
    for (p in own) morloc_release_packet_shm(p)
    if (!is.null(res)) morloc_release_packet_shm(res)
  })
  res <- morloc_foreign_call(sock, as.integer(mid), packets)
  if (is.null(res_codec)) {
    morloc_get_value(res, mlc_closure_schema)
  } else {
    mlc_decode(res, res_codec)
  }
}

# Rebuild a callable from an already-deserialized closure wire tuple
# (home_lang, mid, captured_packets), used when the closure is nested in an
# aggregate whose enclosing get_value has already parsed the tuple. On
# application it encodes its arguments, calls back to the producing pool,
# and decodes the result.
mlc_reflect_from_tuple <- function(tup, arg_codecs, res_codec) {
  home_lang <- tup[[1]]
  mid <- tup[[2]]
  captured <- tup[[3]]
  sock <- paste0(global_state$tmpdir, "/pipe-", home_lang)
  f <- function(...) {
    args <- list(...)
    arg_packets <- lapply(seq_along(args), function(i) mlc_encode(args[[i]], arg_codecs[[i]]))
    mlc_call_released(sock, mid, c(captured, arg_packets), arg_packets, res_codec)
  }
  attr(f, "morloc_origin") <- list(home_lang, as.integer(mid), captured)
  attr(f, "morloc_codecs") <- list(arg_codecs, res_codec)
  f
}

# Apply a function value to its first arguments. A staged function (one that
# does work after these arguments and before the rest) runs that work now,
# once, through its stage entry, and the closure it returns is the result;
# otherwise the arguments are kept with the function, which runs when it gets
# the rest. mlc_stage_table maps a flat entry's manifold id to (context size,
# first stage point, stage entry id).
mlc_papply <- function(f, xs) {
  if (length(xs) == 0) return(f)
  origin <- attr(f, "morloc_origin")
  if (!is.null(origin)) return(mlc_papply_remote(f, origin, xs))
  mid <- attr(f, "morloc_mid")
  if (is.null(mid)) {
    # a function host code made: it has no stages morloc knows of
    return(function(...) do.call(f, c(xs, list(...))))
  }
  caps <- attr(f, "morloc_captured")
  if (is.null(caps)) caps <- list()
  st <- mlc_stage_take(mid, length(caps))
  if (is.null(st) || length(xs) < st$take) return(mlc_pap(mid, caps, xs))
  take <- st$take
  g <- do.call(mlc_manifold(st$stage), c(caps, xs[seq_len(take)]))
  mlc_papply(g, xs[seq_along(xs) > take])
}

# How many more arguments closure `mid`, holding `ncaptured` captured values,
# takes before its stage point, and its stage entry; NULL when it has none.
mlc_stage_take <- function(mid, ncaptured) {
  entry <- mlc_stage_table[[as.character(as.integer(mid))]]
  if (is.null(entry)) return(NULL)
  list(take = entry[[2]] - (ncaptured - entry[[1]]), stage = entry[[3]])
}

# A closure of manifold mid over captured values and applied arguments.
mlc_pap <- function(mid, caps, xs) {
  fn <- mlc_manifold(mid)
  captured <- c(caps, xs)
  g <- function(...) do.call(fn, c(captured, list(...)))
  attr(g, "morloc_mid") <- mid
  attr(g, "morloc_captured") <- captured
  g
}

# The same, for a closure that lives in another pool: its stage entry runs
# there, and the closure it returns comes back as a reflected proxy.
mlc_papply_remote <- function(f, origin, xs) {
  home_lang <- origin[[1]]
  mid <- origin[[2]]
  packets <- origin[[3]]
  codecs <- attr(f, "morloc_codecs")
  arg_codecs <- codecs[[1]]
  res_codec <- codecs[[2]]
  st <- mlc_stage_take(mid, length(packets))
  if (is.null(st) || length(xs) < st$take) {
    more <- lapply(seq_along(xs), function(i) mlc_encode(xs[[i]], arg_codecs[[i]], TRUE))
    return(mlc_reflect_from_tuple(
      list(home_lang, mid, c(packets, more)),
      arg_codecs[seq_along(arg_codecs) > length(xs)], res_codec))
  }
  take <- st$take
  sock <- paste0(global_state$tmpdir, "/pipe-", home_lang)
  own <- lapply(seq_len(take), function(i) mlc_encode(xs[[i]], arg_codecs[[i]]))
  tup <- mlc_call_released(sock, st$stage, c(packets, own), own, NULL)
  g <- mlc_reflect_from_tuple(tup, arg_codecs[seq_along(arg_codecs) > take], res_codec)
  mlc_papply(g, xs[seq_along(xs) > take])
}

# Rebuild a callable from a raw incoming closure wire packet, used when the
# closure is the top-level crossing value (the whole packet is the tuple).
mlc_reflect <- function(pkt, tuple_schema, arg_codecs, res_codec) {
  mlc_reflect_from_tuple(morloc_get_value(pkt, tuple_schema), arg_codecs, res_codec)
}

mlc_make_closure_dispatch <- function(mid, arg_codecs, res_codec) {
  fn <- mlc_manifold(mid)
  function(...) {
    sargs <- list(...)
    args <- lapply(seq_along(sargs), function(i) mlc_decode(sargs[[i]], arg_codecs[[i]]))
    mlc_encode(do.call(fn, args), res_codec)
  }
}

}, envir = .__morloc)

# AUTO include manifolds start
# <<<BREAK>>>
# AUTO include manifolds end

local({

# AUTO include dispatch start
# <<<BREAK>>>
# AUTO include dispatch end

# A new worker, after it is forked: load the user's sources and return the
# dispatch tables it serves calls from.
worker_init <- function() {
  mlc_load_user_sources()
  # If loading failed, every call reports the real cause as a clean fail
  # packet rather than a downstream "could not find function ...".
  if (!is.null(mlc_state$source_error)) {
    error <- mlc_state$source_error
    fail_fn <- function(...) stop(error)
    return(list(lapply(.dispatch, function(f) fail_fn), lapply(.remote_dispatch, function(f) fail_fn)))
  }
  list(.dispatch, .remote_dispatch)
}

main <- function(socket_path, tmpdir, shm_basename) {
  # Force line-buffered stderr so output from user functions is not lost
  # when the nexus kills the pool process group.
  morloc_set_line_buffered()
  # The scheduler (libmorloc) binds the socket, forks the workers and
  # returns the pool's exit status; workers never return from it.
  morloc_fork_pool_run_r(socket_path, tmpdir, shm_basename, worker_init)
}

# The script: the pool serves calls until it is told to stop. A function, so
# its variables are its own and the environment can be locked first.
mlc_start <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  # Health check: confirm sources loaded and print version
  if (length(args) == 1 && args[1] == "--health") {
    # Load the user's sources, so --health also checks that the program's
    # imports and sources resolve.
    mlc_load_user_sources()
    if (!is.null(mlc_state$source_error)) {
      cat('{"status":"error","version":"__MORLOC_VERSION__"}\n')
      quit(status = 1)
    }
    cat('{"status":"ok","version":"__MORLOC_VERSION__"}\n')
    quit(status = 0)
  }

  if (length(args) != 3) {
    cat("Usage: Rscript pool.R <socket_path> <tmpdir> <shm_basename>\n", file=stderr())
    quit(status = 1)
  }

  socket_path <- args[1]
  tmpdir <- args[2]
  shm_basename <- args[3]

  global_state$tmpdir <- tmpdir

  status <- main(socket_path, tmpdir, shm_basename)

  # Use _exit to avoid R cleanup which triggers heap corruption on glibc >= 2.39
  # (R's finalizers attempt to free objects in SHM-related C extensions)
  morloc_exit(status)
}

}, envir = .__morloc)

# R's JIT compiles a small function only where it is global, so the ones
# every call, @try and partial application runs through are compiled here.
local(for (name in c(".mlc_entry", "morloc_get_value", "morloc_put_value", "morloc_release_packet_shm",
                     "morloc_foreign_call", "mlc_encode", "mlc_decode", "mlc_is_closure_codec", "mlc_call_released",
                     "morloc_mlc_try", "mlc_papply", "mlc_pap")) {
  assign(name, compiler::cmpfun(get(name, envir = .__morloc)), envir = .__morloc)
})
lockEnvironment(.__morloc, bindings = TRUE)
# The manifolds' bindings too: a user function that assigns one fails.
local(for (name in .__morloc$mlc_reserved_globals()) lockBinding(name, globalenv()))
.__morloc$mlc_start()
