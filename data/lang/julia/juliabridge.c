/* juliabridge.c -- Thin C bridge between Julia and libmorloc.
 *
 * Compiled to libjuliamorloc.so, called from Julia via ccall.
 * Wraps libmorloc functions that use opaque structs (language_daemon_t,
 * morloc_call_t, Schema) or the ERRMSG pattern into simple pointer/int
 * interfaces that Julia's FFI can handle directly.
 */

#include "morloc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Thread-local error message buffer */
static __thread char jl_errbuf[4096];
static __thread char* jl_errmsg = NULL;

static void clear_err(void) {
    jl_errmsg = NULL;
    jl_errbuf[0] = '\0';
}

/* Get the last error message (returns "" if none). */
const char* jlmorloc_last_error(void) {
    return jl_errmsg ? jl_errmsg : "";
}

/* -- Daemon lifecycle -- */

void* jlmorloc_start_daemon(const char* socket_path, const char* tmpdir,
                             const char* shm_basename, size_t shm_size) {
    clear_err();
    language_daemon_t* d = start_daemon(socket_path, tmpdir, shm_basename,
                                         shm_size, &jl_errmsg);
    if (!d && jl_errmsg) {
        snprintf(jl_errbuf, sizeof(jl_errbuf), "%s", jl_errmsg);
        jl_errmsg = jl_errbuf;
    }
    return (void*)d;
}

void jlmorloc_close_daemon(void* daemon) {
    language_daemon_t* d = (language_daemon_t*)daemon;
    close_daemon(&d);
}

int jlmorloc_wait_for_client(void* daemon) {
    clear_err();
    return wait_for_client((language_daemon_t*)daemon, &jl_errmsg);
}

/* -- Packet I/O -- */

/* Returns a pointer to the packet bytes. Caller must NOT free this
 * directly -- it lives in shared memory or was allocated by libmorloc. */
uint8_t* jlmorloc_stream_from_client(int client_fd, size_t* out_size) {
    clear_err();
    uint8_t* pkt = stream_from_client(client_fd, &jl_errmsg);
    if (pkt && out_size) {
        /* Packet size is in the first 4 bytes (little-endian uint32) */
        uint32_t sz;
        memcpy(&sz, pkt, sizeof(sz));
        *out_size = (size_t)sz;
    }
    return pkt;
}

int jlmorloc_send_packet(int client_fd, uint8_t* packet) {
    clear_err();
    size_t sent = send_packet_to_foreign_server(client_fd, packet, &jl_errmsg);
    return sent > 0 ? 0 : -1;
}

void jlmorloc_close_socket(int fd) {
    close_socket(fd);
}

/* -- Packet classification -- */

int jlmorloc_is_ping(const uint8_t* packet) {
    clear_err();
    return packet_is_ping(packet, &jl_errmsg) ? 1 : 0;
}

int jlmorloc_is_local_call(const uint8_t* packet) {
    clear_err();
    return packet_is_local_call(packet, &jl_errmsg) ? 1 : 0;
}

int jlmorloc_is_remote_call(const uint8_t* packet) {
    clear_err();
    return packet_is_remote_call(packet, &jl_errmsg) ? 1 : 0;
}

uint8_t* jlmorloc_pong(const uint8_t* packet) {
    clear_err();
    return return_ping(packet, &jl_errmsg);
}

/* -- Call packet parsing -- */

/* Parse a call packet. Returns the manifold index via out_mid,
 * the number of arguments via out_nargs, and a pointer to the
 * morloc_call_t (which the caller must free via jlmorloc_free_call). */
void* jlmorloc_read_call(const uint8_t* packet, uint32_t* out_mid,
                          size_t* out_nargs) {
    clear_err();
    morloc_call_t* call = read_morloc_call_packet(packet, &jl_errmsg);
    if (!call) return NULL;
    *out_mid = call->midx;
    *out_nargs = call->nargs;
    return (void*)call;
}

/* Get the i-th argument packet from a parsed call. */
uint8_t* jlmorloc_call_arg(void* call_ptr, size_t i) {
    morloc_call_t* call = (morloc_call_t*)call_ptr;
    if (i >= call->nargs) return NULL;
    return call->args[i];
}

void jlmorloc_free_call(void* call_ptr) {
    if (call_ptr) free_morloc_call((morloc_call_t*)call_ptr);
}

/* -- Msgpack bridge -- */

/* Convert msgpack bytes + schema string -> morloc data packet.
 * The schema_str is a compact type descriptor like "i4", "ai4", "m{x:f8}". */
uint8_t* jlmorloc_pack(const char* mpk, size_t mpk_size,
                        const char* schema_str) {
    clear_err();
    Schema* schema = parse_schema(schema_str, &jl_errmsg);
    if (!schema) return NULL;
    uint8_t* pkt = make_data_packet_from_mpk(mpk, mpk_size, schema);
    free_schema(schema);
    return pkt;
}

/* Convert a morloc data packet -> msgpack bytes.
 * Returns a malloc'd buffer; caller must free it. */
char* jlmorloc_unpack(const uint8_t* packet, const char* schema_str,
                       size_t* out_size) {
    clear_err();
    Schema* schema = parse_schema(schema_str, &jl_errmsg);
    if (!schema) return NULL;
    char* mpk = NULL;
    size_t mpk_size = 0;
    int ok = get_data_packet_as_mpk(packet, schema, &mpk, &mpk_size, &jl_errmsg);
    free_schema(schema);
    if (!ok) return NULL;
    *out_size = mpk_size;
    return mpk;
}

/* -- Error packet -- */

uint8_t* jlmorloc_make_fail_packet(const char* msg) {
    return make_fail_packet(msg);
}

/* -- Foreign call (cross-pool IPC) -- */

uint8_t* jlmorloc_foreign_call(const char* tmpdir, const char* socket_name,
                                uint32_t mid, uint8_t** arg_packets,
                                size_t nargs) {
    clear_err();
    /* Build the call packet */
    uint8_t* call_pkt = make_morloc_local_call_packet(
        mid, (const uint8_t**)arg_packets, nargs, &jl_errmsg);
    if (!call_pkt) return NULL;

    /* Build the socket path */
    size_t pathlen = strlen(tmpdir) + 1 + strlen(socket_name) + 1;
    char* socket_path = (char*)malloc(pathlen);
    snprintf(socket_path, pathlen, "%s/%s", tmpdir, socket_name);

    /* Send and receive */
    uint8_t* result = send_and_receive_over_socket(socket_path, call_pkt,
                                                    &jl_errmsg);
    free(socket_path);
    free(call_pkt);
    return result;
}

/* -- Shared memory init (needed before daemon start in some cases) -- */

int jlmorloc_shinit(const char* basename, int volume, size_t size) {
    clear_err();
    return shinit(basename, volume, size, &jl_errmsg) ? 0 : -1;
}

void jlmorloc_set_fallback_dir(const char* dir) {
    shm_set_fallback_dir(dir);
}
