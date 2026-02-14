#ifndef __MORLOC_ROUTER_H__
#define __MORLOC_ROUTER_H__

#include "macros.h"
#include "daemon.h"
#include "manifest.h"

#include <stdbool.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/un.h>

typedef struct router_program_s {
    char* name;
    char* manifest_path;
    manifest_t* manifest;
    pid_t daemon_pid;            // 0 if not started
    char daemon_socket[sizeof(((struct sockaddr_un*)0)->sun_path)];
} router_program_t;

typedef struct router_s {
    router_program_t* programs;
    size_t n_programs;
    char* fdb_path;
} router_t;

// Scan fdb/ directory and build program registry
router_t* router_init(const char* fdb_path, ERRMSG);

// Main router event loop (listens on configured endpoints, forwards to daemons)
void router_run(daemon_config_t* config, router_t* router);

// Start a program daemon as a child process
bool router_start_program(router_program_t* prog, ERRMSG);

// Forward a request to the appropriate program daemon
daemon_response_t* router_forward(router_t* router, const char* program,
                                   daemon_request_t* request, ERRMSG);

// Build unified discovery JSON across all programs
char* router_build_discovery(router_t* router);

// Free router resources
void router_free(router_t* router);

#endif // __MORLOC_ROUTER_H__
