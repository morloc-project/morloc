#ifndef __MORLOC_POOL_H__
#define __MORLOC_POOL_H__

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Dispatch callback: given mid + arg packets, return result packet.
// ctx is an opaque pointer to language-specific dispatch state:
//   - C++: NULL (dispatch is a plain switch statement)
//   - Python: PyObject* dict mapping mid -> callable
//   - R: SEXP list of dispatch functions
// The callback MUST NOT throw (C++ callbacks must wrap in try/catch).
// The callback MUST return a valid packet (data or fail), never NULL.
typedef uint8_t* (*pool_dispatch_fn_t)(
    uint32_t mid,
    const uint8_t** args,
    size_t nargs,
    void* ctx
);

typedef enum {
    POOL_THREADS,  // C++: pthreads + mutex/cond job queue
    POOL_FORK,     // Python/R: fork + socketpair fd-passing
    POOL_SINGLE    // single-threaded sequential processing
} pool_concurrency_t;

typedef struct {
    pool_dispatch_fn_t local_dispatch;
    pool_dispatch_fn_t remote_dispatch;
    void* dispatch_ctx;          // opaque, passed to callbacks
    pool_concurrency_t concurrency;
    int initial_workers;         // default: 1
    bool dynamic_scaling;        // spawn workers when all busy
    void (*post_fork_child)(void* ctx); // called in child after fork (POOL_FORK only)
} pool_config_t;

// Opaque handle returned by pool internals
typedef struct pool_state_s pool_state_t;

// Run the complete pool daemon lifecycle.
// argv: [program_name, socket_path, tmpdir, shm_basename]
int pool_main(int argc, char** argv, pool_config_t* config);

// Classify and dispatch a single packet.
// Handles ping->pong internally. Always returns a valid packet
// (data or fail), never NULL. Caller must free the result.
uint8_t* pool_dispatch_packet(
    const uint8_t* packet,
    pool_dispatch_fn_t local_dispatch,
    pool_dispatch_fn_t remote_dispatch,
    void* ctx
);

// Busy-tracking for dynamic worker spawning.
// Language bindings call these around foreign_call.
// These operate on the global pool state set by pool_main().
void pool_mark_busy(void);
void pool_mark_idle(void);

#ifdef __cplusplus
}
#endif

#endif // __MORLOC_POOL_H__
