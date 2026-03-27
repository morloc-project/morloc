#ifndef __MORLOC_DAEMON_H__
#define __MORLOC_DAEMON_H__

#include "macros.h"
#include "call.h"
#include "manifest.h"
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

// ======================================================================
// Binding store: content-addressed compiled expression cache
// ======================================================================

// A single binding: maps a hash to compiled artifacts on disk
typedef struct binding_entry_s {
    uint64_t hash;             // xxhash of normalized expression
    char* expr;                // original expression text
    char* artifact_dir;        // path to compiled artifacts directory
    char* type_sig;            // type signature (from typecheck)
    char** names;              // human-readable aliases
    size_t n_names;            // number of aliases
} binding_entry_t;

// The binding store: hash table of compiled expressions + name index
typedef struct binding_store_s {
    binding_entry_t* entries;  // hash-indexed array (linear probe)
    size_t capacity;           // total slots
    size_t count;              // occupied slots
    char* base_dir;            // root directory for artifacts
    char* names_path;          // path to names.json file
} binding_store_t;

// Initialize a binding store rooted at the given directory
binding_store_t* binding_store_init(const char* base_dir);

// Free a binding store
void binding_store_free(binding_store_t* store);

// Look up a binding by expression hash
binding_entry_t* binding_store_lookup_hash(binding_store_t* store, uint64_t hash);

// Look up a binding by human-readable name
binding_entry_t* binding_store_lookup_name(binding_store_t* store, const char* name);

// Add a binding (compiles expression if not already cached)
// Returns the entry (existing or newly created)
binding_entry_t* binding_store_bind(
    binding_store_t* store,
    const char* expr,
    const char* name,  // optional, can be NULL
    int eval_timeout
);

// List all bindings as JSON (caller must free)
char* binding_store_list_json(binding_store_t* store);

// Remove a name alias. If no names remain, artifacts are deleted.
bool binding_store_unbind(binding_store_t* store, const char* name);

// Callback to check and restart dead pools (called from daemon event loop)
typedef void (*pool_check_fn_t)(morloc_socket_t* sockets, size_t n_pools);

// Callback to check if pool i is alive (returns true if alive)
typedef bool (*pool_alive_fn_t)(size_t pool_index);

// Listener configuration
typedef struct daemon_config_s {
    const char* unix_socket_path;  // --socket <path>
    int tcp_port;                  // --port <n>  (0 = disabled)
    int http_port;                 // --http-port <n>  (0 = disabled)
    pool_check_fn_t pool_check_fn; // optional pool health check callback
    pool_alive_fn_t pool_alive_fn; // optional pool liveness check
    size_t n_pools;                // number of pools (for health check)
    int eval_timeout;              // --eval-timeout <seconds> (0 = disabled, default 30)
} daemon_config_t;

// Request method
typedef enum {
    DAEMON_CALL,
    DAEMON_DISCOVER,
    DAEMON_HEALTH,
    DAEMON_EVAL,       // POST /eval -- compose and run a morloc expression
    DAEMON_TYPECHECK,  // POST /typecheck -- dry-run type validation
    DAEMON_BIND,       // POST /bind -- compile and save a binding
    DAEMON_BINDINGS,   // GET /bindings -- list all bindings
    DAEMON_UNBIND      // DELETE /bindings/<name> -- remove a binding
} daemon_method_t;

// Incoming request (parsed from JSON or HTTP)
typedef struct daemon_request_s {
    char* id;              // optional request ID for correlation
    daemon_method_t method;
    char* command;         // command name (for CALL/UNBIND)
    char* args_json;       // raw JSON array of args (for CALL)
    char* expr;            // morloc expression (for EVAL/BIND)
    char* name;            // binding name (for BIND/UNBIND)
} daemon_request_t;

// Result of dispatching a request
typedef struct daemon_response_s {
    char* id;              // echoed from request
    bool success;
    char* result_json;     // JSON result (on success)
    char* error;           // error message (on failure)
} daemon_response_t;

// Main entry point: runs the daemon event loop (does not return until shutdown)
void daemon_run(daemon_config_t* config, manifest_t* manifest,
                morloc_socket_t* sockets, size_t n_pools,
                const char* shm_basename);

// Dispatch a single request against a loaded manifest
daemon_response_t* daemon_dispatch(manifest_t* manifest,
                                    daemon_request_t* request,
                                    morloc_socket_t* sockets,
                                    const char* shm_basename);

// Parse a JSON request body into a daemon_request_t
daemon_request_t* daemon_parse_request(const char* json, size_t len, ERRMSG);

// Parse a JSON response body into a daemon_response_t
daemon_response_t* daemon_parse_response(const char* json, size_t len, ERRMSG);

// Serialize a daemon_response_t to JSON (caller must free result)
char* daemon_serialize_response(daemon_response_t* response, size_t* out_len);

// Build discovery JSON from manifest (caller must free result)
char* daemon_build_discovery(manifest_t* manifest);

// Set the eval timeout (seconds) for DAEMON_EVAL dispatch.
// Must be called before daemon_dispatch() with DAEMON_EVAL requests.
void daemon_set_eval_timeout(int timeout_sec);

// Cleanup
void daemon_free_request(daemon_request_t* req);
void daemon_free_response(daemon_response_t* resp);

#endif // __MORLOC_DAEMON_H__
