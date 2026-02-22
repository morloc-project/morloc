#ifndef __MORLOC_H__
#define __MORLOC_H__

#include <assert.h>
#include <ctype.h>
#include <dirent.h> // used in delete_directory
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>      // For calloc, free
#include <string.h>      // For memcpy, strerror
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

// needed for interop
#include <poll.h>
#include <signal.h>      // For sigprocmask, sigset_t
#include <sys/select.h>  // For pselect, fd_set
#include <sys/socket.h>  // For recv
#include <sys/un.h>
#include <sys/wait.h>

#include "packet.h"
#include "memory.h"
#include "schema.h"
#include "eval.h"
#include "call.h"
#include "slurm.h"
#include "utility.h"
#include "manifest.h"
#include "daemon.h"
#include "pool.h"
#include "http.h"
#include "router.h"

#ifdef __cplusplus
extern "C" {
#endif

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG);
shm_t* shopen(size_t volume_index, ERRMSG);
bool shclose(ERRMSG);
void shm_set_fallback_dir(const char* dir);
void* shmalloc(size_t size, ERRMSG);
void* shmemcpy(void* src, size_t size, ERRMSG);
bool shfree(absptr_t ptr, ERRMSG);
void* shcalloc(size_t nmemb, size_t size, ERRMSG);
void* shrealloc(void* ptr, size_t size, ERRMSG);
size_t total_shm_size();
volptr_t rel2vol(relptr_t ptr, ERRMSG);
absptr_t rel2abs(relptr_t ptr, ERRMSG);
relptr_t vol2rel(volptr_t ptr, shm_t* shm);
absptr_t vol2abs(volptr_t ptr, shm_t* shm);
volptr_t abs2vol(absptr_t ptr, shm_t* shm, ERRMSG);
relptr_t abs2rel(absptr_t ptr, ERRMSG);
shm_t* abs2shm(absptr_t ptr, ERRMSG);
block_header_t* abs2blk(void* ptr, ERRMSG);
Schema* parse_schema(const char* schema, ERRMSG);
Schema* parse_schema_r(char** schema_ptr, ERRMSG);
int pack(const void* mlc, const char* schema_str, char** mpkptr, size_t* mpk_size, ERRMSG);
int pack_with_schema(const void* mlc, const Schema* schema, char** mpkptr, size_t* mpk_size, ERRMSG);
int unpack(const char* mpk, size_t mpk_size, const char* schema_str, void** mlcptr, ERRMSG);
int unpack_with_schema(const char* mpk, size_t mpk_size, const Schema* schema, void** mlcptr, ERRMSG);
char* schema_to_string(const Schema* schema);
void* get_ptr(const Schema* schema, ERRMSG);
void free_schema(Schema* schema);
char* quoted(const char* input);
bool print_voidstar(const void* voidstar, const Schema* schema, ERRMSG);
morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG);
bool packet_is_ping(const uint8_t* packet, ERRMSG);
bool packet_is_local_call(const uint8_t* packet, ERRMSG);
bool packet_is_remote_call(const uint8_t* packet, ERRMSG);
size_t morloc_packet_size_from_header(const morloc_packet_header_t* header);
size_t morloc_packet_size(const uint8_t* packet, ERRMSG);
uint8_t* return_ping(const uint8_t* packet, ERRMSG);
uint8_t* make_ping_packet();
uint8_t* make_standard_data_packet(relptr_t ptr, const Schema* schema);
uint8_t* make_mpk_data_packet(const char* mpk_filename, const Schema* schema);
uint8_t* make_data_packet_from_mpk(const char* mpk, size_t mpk_size, const Schema* schema);
int get_data_packet_as_mpk(const uint8_t* packet, const Schema* schema, char** mpk_out, size_t* mpk_size_out, ERRMSG);
morloc_metadata_header_t* as_morloc_metadata_header(const uint8_t* ptr);
char* read_schema_from_packet_meta(const uint8_t* packet, ERRMSG);
uint8_t* make_fail_packet(const char* failure_message);
char* get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG);
uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG);
uint8_t* make_morloc_local_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
uint8_t* make_morloc_remote_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG);
void free_morloc_call(morloc_call_t* call);
int print_morloc_data_packet(const uint8_t* packet, const Schema* schema, ERRMSG);
void close_socket(int socket_id);
void close_daemon(language_daemon_t** daemon_ptr);
language_daemon_t* start_daemon( const char* socket_path, const char* tmpdir, const char* shm_basename, size_t shm_default_size, ERRMSG );
uint8_t* stream_from_client_wait(int client_fd, int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* stream_from_client(int client_fd, ERRMSG);
uint8_t* send_and_receive_over_socket_wait(const char* socket_path, const uint8_t* packet, int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* send_and_receive_over_socket(const char* socket_path, const uint8_t* packet, ERRMSG);
size_t send_packet_to_foreign_server(int client_fd, uint8_t* packet, ERRMSG);
int wait_for_client_with_timeout(language_daemon_t* daemon, int timeout_us, ERRMSG);
int wait_for_client(language_daemon_t* daemon, ERRMSG);
bool hash_morloc_packet(const uint8_t* packet, const Schema* schema, uint64_t seed, uint64_t* hash, ERRMSG);
char* put_cache_packet(const uint8_t* voidstar, const Schema* schema, uint64_t key, const char* cache_path, ERRMSG);
uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
char* check_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
argument_t* initialize_positional(char* value);
argument_t* initialize_unrolled(size_t size, char* default_value, char** fields, char** default_fields);
void free_argument_t(argument_t* arg);
uint8_t* parse_cli_data_argument(uint8_t* dest, const argument_t* arg, const Schema* schema, ERRMSG);
uint8_t* make_call_packet_from_cli( uint8_t* dest, uint32_t mid, argument_t** args, char** arg_schema_strs, ERRMSG );
morloc_expression_t* make_morloc_bound_var(const char* schema_str, char* varname, ERRMSG);
morloc_expression_t* make_morloc_literal( const char* schema_str, primitive_t lit, ERRMSG );
morloc_expression_t* make_morloc_container( const char* schema_str, ERRMSG, size_t nargs, ... );
morloc_expression_t* make_morloc_app( const char* schema_str, morloc_expression_t* func, ERRMSG, size_t nargs, ... );
morloc_expression_t* make_morloc_lambda( morloc_expression_t* body, size_t nvars, ... );
morloc_expression_t* make_morloc_interpolation(const char* schema_str, ERRMSG, size_t nargs, ...);
morloc_expression_t* make_morloc_pattern(const char* schema_str, morloc_pattern_t* pattern, ERRMSG);
morloc_pattern_t* make_morloc_pattern_end();
morloc_pattern_t* make_morloc_pattern_idx(size_t nargs, ...);
morloc_pattern_t* make_morloc_pattern_key(size_t nargs, ...);
absptr_t morloc_eval( morloc_expression_t* expr, Schema* return_schema, uint8_t** arg_voidstar, Schema** arg_schemas, size_t nargs, ERRMSG );


// Parse a slurm walltime string
//
// For now, in morloc, I will be fairly strict and allow only two forms:
//  1. days-hours:minutes:seconds
//  1. hours:minutes:seconds
//
// Examples:
//   * 5-12:00:00 - 5 and a half days
//   * 12:00:00 - 12 hours
//   * 00:30:00 - 30 minutes
size_t parse_slurm_time(const char* time_str, ERRMSG);

// Convert a number of seconds to a walltime string of format:
//   days-hours:minutes:seconds
// hours, minutes, and seconds should be 0-padded to 2 digits
char* write_slurm_time(int seconds);

// Parse the arguments of a morloc call packet.
//
// This function mutates the `args ` and `nargs` arguments to store the call
// arguments and counts (respectively). `args` contains pointers to locations
// in the original `packet` array, so `packet` must not be freed until after
// `args` is freed.
bool parse_morloc_call_arguments( uint8_t* packet, uint8_t** args, size_t* nargs, ERRMSG );

bool slurm_job_is_complete(uint32_t job_id);

uint32_t submit_morloc_slurm_job(
  const char* nexus_path,
  const char* socket_basename,
  const char* call_packet_filename,
  const char* result_cache_filename,
  const char* output_filename,
  const char* error_filename,
  const resources_t* resources,
  ERRMSG
);

uint8_t* remote_call(
  int midx,
  const char* socket_basename,
  const char* cache_path,
  const resources_t* resources,
  const uint8_t** arg_packets,
  size_t nargs,
  ERRMSG
);

// xxhash wrapper - avoids exposing xxhash.h in public header
uint64_t morloc_xxh64(const void* input, size_t length, uint64_t seed);

// manifest reader
manifest_t* parse_manifest(const char* text, ERRMSG);
manifest_t* read_manifest(const char* path, ERRMSG);
void free_manifest(manifest_t* manifest);
morloc_expression_t* build_manifest_expr(const char* json_str, ERRMSG);

#ifdef __cplusplus
}
#endif

#endif // __MORLOC_H__
