#ifndef __MORLOC_DAEMON_H__
#define __MORLOC_DAEMON_H__

#include "macros.h"
#include "call.h"
#include "manifest.h"
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

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
} daemon_config_t;

// Request method
typedef enum {
    DAEMON_CALL,
    DAEMON_DISCOVER,
    DAEMON_HEALTH
} daemon_method_t;

// Incoming request (parsed from JSON or HTTP)
typedef struct daemon_request_s {
    char* id;              // optional request ID for correlation
    daemon_method_t method;
    char* command;         // command name (for CALL)
    char* args_json;       // raw JSON array of args (for CALL)
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

// Cleanup
void daemon_free_request(daemon_request_t* req);
void daemon_free_response(daemon_response_t* resp);

#endif // __MORLOC_DAEMON_H__
