// morloc.h -- C ABI contract for libmorloc.so
//
// This is the single public header for consumers of the morloc runtime library.
// It defines all types and function declarations exported by libmorloc.so.

#ifndef __MORLOC_H__
#define __MORLOC_H__

#ifdef __cplusplus
extern "C" {
#endif

// ========================================================================
// Section 1: System includes and basic typedefs
// ========================================================================

#include <stdarg.h>     // va_list, va_start, va_arg, va_end (used by pool templates)
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>      // FILE* for read_binary_fd
#include <sys/select.h> // fd_set
#include <sys/socket.h>
#include <sys/types.h>  // pid_t, ssize_t
#include <sys/un.h>     // struct sockaddr_un

// Error message output parameter: all fallible functions take this as their
// last argument. On failure the callee sets *errmsg_ to a heap-allocated
// string describing the error. On success *errmsg_ is set to NULL.
typedef char** errmsg_;
#define ERRMSG char** errmsg_

// Exit codes used by several functions.
#define EXIT_PASS 0
#define EXIT_FAIL 1

// Convenience macros used by language extensions (pymorloc.c, rmorloc.c, cppmorloc.cpp)
#include <stdlib.h>  // free
#define FREE(ptr) if(ptr != NULL){ free(ptr); ptr = NULL; }

// Size limits shared between library and consumers.
#define MAX_FILENAME_SIZE 128
#define MAX_ERRMSG_SIZE   1024
#define MAX_PATH_SIZE     512
#define BUFFER_SIZE       4096

// Opaque JSON builder handle (Rust uses its own struct internally).
typedef void* json_buf_t;

// ========================================================================
// Section 2: Memory / SHM types
// ========================================================================

// Pointer types for the multi-volume shared memory pool.
//
// relptr_t  -- index into the logical (multi-volume) pool; shared between
//              processes.
// volptr_t  -- index into a single volume (0 = first block after shm header).
// absptr_t  -- absolute virtual address in the current process.
typedef ssize_t relptr_t;
typedef ssize_t volptr_t;
typedef void*   absptr_t;

#define VOLNULL -1
#define RELNULL -1

// Magic numbers for integrity checks.
#define SHM_MAGIC 0xFECA0DF0
#define BLK_MAGIC 0x0CB10DF0

#define MAX_VOLUME_NUMBER 32

// Shared memory volume header (lives at the start of each mmap'd region).
typedef struct shm_s {
    unsigned int magic;
    char volume_name[MAX_FILENAME_SIZE];
    int volume_index;
    size_t volume_size;
    size_t relative_offset;
    // Note: pthread_rwlock_t is opaque; consumers should not access it directly.
    // It is included here so that sizeof(shm_t) is correct for mmap calculations.
    // On Linux x86_64 this is typically 56 bytes.
    char _rwlock_storage[56]; // placeholder for pthread_rwlock_t
    volptr_t cursor;
} shm_t;

// Block header preceding every allocation inside a shared memory volume.
// Atomic reference count for thread safety. Layout is stable (no padding).
typedef struct block_header_s {
    unsigned int magic;
    unsigned int reference_count; // actually _Atomic in the C impl
    size_t size;
} block_header_t;

// ========================================================================
// Section 3: Schema types
// ========================================================================

typedef enum {
    MORLOC_NIL,
    MORLOC_BOOL,
    MORLOC_SINT8,
    MORLOC_SINT16,
    MORLOC_SINT32,
    MORLOC_SINT64,
    MORLOC_UINT8,
    MORLOC_UINT16,
    MORLOC_UINT32,
    MORLOC_UINT64,
    MORLOC_FLOAT32,
    MORLOC_FLOAT64,
    MORLOC_TENSOR,
    MORLOC_STRING,
    MORLOC_ARRAY,
    MORLOC_TUPLE,
    MORLOC_MAP,
    MORLOC_OPTIONAL
} morloc_serial_type;

// Single-character schema encoding tokens.
#define SCHEMA_NIL      'z'
#define SCHEMA_BOOL     'b'
#define SCHEMA_SINT     'i'
#define SCHEMA_UINT     'u'
#define SCHEMA_FLOAT    'f'
#define SCHEMA_STRING   's'
#define SCHEMA_ARRAY    'a'
#define SCHEMA_TENSOR   'T'
#define SCHEMA_TUPLE    't'
#define SCHEMA_MAP      'm'
#define SCHEMA_OPTIONAL '?'

// Schema: recursive type descriptor used for serialisation/deserialisation.
struct Schema;
typedef struct Schema {
    morloc_serial_type type;
    size_t size;       // number of parameters
    size_t width;      // bytes per element when stored in a fixed-width array
    size_t* offsets;   // field offsets (tuples) or ndim (tensors, in offsets[0])
    char* hint;
    struct Schema** parameters;
    char** keys;       // field names (records only)
} Schema;

// Variable-length array in voidstar representation.
typedef struct Array {
    size_t size;
    relptr_t data;
} Array;

// Dense N-dimensional tensor in voidstar representation (row-major / C order).
typedef struct Tensor {
    size_t total_elements;
    uint32_t device_type;   // reserved: 0 = CPU
    uint32_t device_id;     // reserved: 0
    relptr_t data;          // relptr to contiguous element data
    relptr_t shape;         // relptr to int64_t[ndim]
} Tensor;

// ========================================================================
// Section 4: Packet types
// ========================================================================

#define MORLOC_PACKET_MAGIC 0x0707f86d

// Packet type discriminator.
typedef uint8_t command_type_t;
#define PACKET_TYPE_DATA  ((command_type_t)0)
#define PACKET_TYPE_CALL  ((command_type_t)1)
#define PACKET_TYPE_PING  ((command_type_t)2)

// Packed command sub-structs (all 8 bytes wide).
typedef struct __attribute__((packed)) packet_command_type_s {
    command_type_t type;
    uint8_t padding[7];
} packet_command_type_t;

#define PACKET_ENTRYPOINT_LOCAL      0x00
#define PACKET_ENTRYPOINT_REMOTE_SFS 0x01

typedef struct __attribute__((packed)) packet_command_call_s {
    command_type_t type;
    uint8_t entrypoint;
    uint8_t padding[2];
    uint32_t midx;
} packet_command_call_t;

// Data source, format, compression, encryption, and status constants.
#define PACKET_SOURCE_MESG  0x00
#define PACKET_SOURCE_FILE  0x01
#define PACKET_SOURCE_RPTR  0x02

#define PACKET_FORMAT_JSON     0x00
#define PACKET_FORMAT_MSGPACK  0x01
#define PACKET_FORMAT_TEXT     0x02
#define PACKET_FORMAT_DATA     0x03
#define PACKET_FORMAT_VOIDSTAR 0x04
#define PACKET_FORMAT_ARROW    0x05

#define PACKET_COMPRESSION_NONE 0x00
#define PACKET_ENCRYPTION_NONE  0x00

#define PACKET_STATUS_PASS 0x00
#define PACKET_STATUS_FAIL 0x01

typedef struct __attribute__((packed)) packet_command_data_s {
    command_type_t type;
    uint8_t source;
    uint8_t format;
    uint8_t compression;
    uint8_t encryption;
    uint8_t status;
    uint8_t padding[2];
} packet_command_data_t;

typedef struct __attribute__((packed)) packet_command_ping_s {
    command_type_t type;
    uint8_t padding[7];
} packet_command_ping_t;

typedef union __attribute__((packed)) packet_command_u {
    packet_command_type_t cmd_type;
    packet_command_call_t call;
    packet_command_data_t data;
    packet_command_ping_t ping;
} packet_command_t;

// 32-byte packet header (stable binary format, packed).
typedef struct __attribute__((packed)) morloc_packet_header_s {
    uint32_t magic;
    uint16_t plain;
    uint16_t version;
    uint16_t flavor;
    uint16_t mode;
    packet_command_t command;
    uint32_t offset;
    uint64_t length;
} morloc_packet_header_t;

// Inline threshold: voidstar data <= this size is embedded in packet payload.
#define MORLOC_INLINE_THRESHOLD (64 * 1024)

// Metadata sub-header in packet metadata sections.
#define MORLOC_METADATA_TYPE_SCHEMA_STRING 0x01
#define MORLOC_METADATA_TYPE_XXHASH        0x02
#define MORLOC_METADATA_HEADER_MAGIC       "mmh"

typedef struct __attribute__((packed)) morloc_metadata_header_s {
    char magic[3];
    uint8_t type;
    uint32_t size;
} morloc_metadata_header_t;

// ========================================================================
// Section 5: Expression / eval types
// ========================================================================

typedef struct argument_s {
    char* value;
    char** fields;
    char** default_fields;
    size_t size;
} argument_t;

typedef enum {
    MORLOC_X_DAT,
    MORLOC_X_APP,
    MORLOC_X_LAM,
    MORLOC_X_BND,
    MORLOC_X_PAT,
    MORLOC_X_FMT,
    MORLOC_X_SHOW,
    MORLOC_X_READ
} morloc_expression_type;

typedef enum { APPLY_PATTERN, APPLY_LAMBDA, APPLY_FORMAT } morloc_app_expression_type;

typedef enum { SELECT_BY_KEY, SELECT_BY_INDEX, SELECT_END } morloc_pattern_type;

// Forward declarations.
typedef struct morloc_expression_s morloc_expression_t;
typedef struct morloc_app_expression_s morloc_app_expression_t;
typedef struct morloc_lam_expression_s morloc_lam_expression_t;
typedef struct morloc_data_s morloc_data_t;
typedef struct morloc_pattern_s morloc_pattern_t;

typedef union primitive_u {
    char*    s;
    uint8_t  z;
    bool     b;
    int8_t   i1;
    int16_t  i2;
    int32_t  i4;
    int64_t  i8;
    uint8_t  u1;
    uint16_t u2;
    uint32_t u4;
    uint64_t u8;
    float    f4;
    double   f8;
} primitive_t;

typedef struct morloc_data_array_s {
    Schema* schema;
    size_t size;
    morloc_expression_t** values;
} morloc_data_array_t;

typedef struct morloc_data_s {
    bool is_voidstar;
    union {
        primitive_t lit_val;
        morloc_expression_t** tuple_val;
        morloc_data_array_t* array_val;
        void* voidstar;
    } data;
} morloc_data_t;

typedef struct morloc_app_expression_s {
    morloc_app_expression_type type;
    union {
        morloc_pattern_t* pattern;
        morloc_lam_expression_t* lambda;
        char** fmt;
    } function;
    morloc_expression_t** args;
    size_t nargs;
} morloc_app_expression_t;

typedef struct morloc_lam_expression_s {
    size_t nargs;
    char** args;
    morloc_expression_t* body;
} morloc_lam_expression_t;

typedef struct morloc_pattern_s {
    morloc_pattern_type type;
    size_t size;
    union {
        size_t* indices;
        char** keys;
    } fields;
    morloc_pattern_t** selectors;
} morloc_pattern_t;

typedef struct morloc_expression_s {
    morloc_expression_type type;
    Schema* schema;
    union {
        morloc_app_expression_t* app_expr;
        morloc_lam_expression_t* lam_expr;
        char* bnd_expr;
        char** interpolation;
        morloc_pattern_t* pattern_expr;
        morloc_data_t* data_expr;
        morloc_expression_t* unary_expr;
    } expr;
} morloc_expression_t;

// ========================================================================
// Section 6: Manifest types
// ========================================================================

typedef struct {
    char* lang;
    char** exec;      // NULL-terminated array
    char* socket;     // socket basename
} manifest_pool_t;

typedef enum {
    MARG_POS = 0,
    MARG_OPT,
    MARG_FLAG,
    MARG_GRP
} manifest_arg_kind_t;

typedef struct manifest_arg_s manifest_arg_t;

typedef struct {
    char* key;
    manifest_arg_t* arg;
} manifest_grp_entry_t;

struct manifest_arg_s {
    manifest_arg_kind_t kind;
    char** desc;
    char* metavar;
    char* type_desc;
    bool quoted;
    char short_opt;
    char* long_opt;
    char* long_rev;
    char* default_val;
    char grp_short;
    char* grp_long;
    manifest_grp_entry_t* entries;
    size_t n_entries;
};

typedef struct {
    char* name;
    char** desc;
} manifest_cmd_group_t;

typedef struct {
    char* name;
    bool is_pure;
    uint32_t mid;
    size_t pool_index;
    size_t* needed_pools;
    size_t n_needed_pools;
    char** arg_schemas;
    char* return_schema;
    char** desc;
    char* return_type;
    char** return_desc;
    manifest_arg_t* args;
    size_t n_args;
    morloc_expression_t* expr;
    char* group;
} manifest_command_t;

typedef struct {
    char* type;
    char* host;
    int port;
    char* socket;
} manifest_service_t;

typedef struct {
    int version;
    char* name;
    char* build_dir;
    manifest_pool_t* pools;
    size_t n_pools;
    manifest_command_t* commands;
    size_t n_commands;
    manifest_cmd_group_t* groups;
    size_t n_groups;
    manifest_service_t* service;
} manifest_t;

// ========================================================================
// Section 7: Daemon / HTTP / Router types
// ========================================================================

// -- Call types --

typedef struct morloc_call_s {
    uint32_t midx;
    uint8_t** args;
    size_t nargs;
    int owns_args;
} morloc_call_t;

typedef struct client_list_s {
    int fd;
    struct client_list_s* next;
} client_list_t;

typedef struct language_daemon_s {
    char* socket_path;
    char* tmpdir;
    char* shm_basename;
    shm_t* shm;
    size_t shm_default_size;
    int server_fd;
    fd_set read_fds;
    client_list_t* client_fds;
} language_daemon_t;

typedef struct morloc_socket_s {
    char* lang;
    char** syscmd;
    char* socket_filename;
    int pid;
} morloc_socket_t;

// -- Binding store --

typedef struct binding_entry_s {
    uint64_t hash;
    char* expr;
    char* artifact_dir;
    char* type_sig;
    char** names;
    size_t n_names;
} binding_entry_t;

typedef struct binding_store_s {
    binding_entry_t* entries;
    size_t capacity;
    size_t count;
    char* base_dir;
    char* names_path;
} binding_store_t;

// -- Daemon config and request/response --

typedef void (*pool_check_fn_t)(morloc_socket_t* sockets, size_t n_pools);
typedef bool (*pool_alive_fn_t)(size_t pool_index);

typedef struct daemon_config_s {
    const char* unix_socket_path;
    int tcp_port;
    int http_port;
    pool_check_fn_t pool_check_fn;
    pool_alive_fn_t pool_alive_fn;
    size_t n_pools;
    int eval_timeout;
} daemon_config_t;

typedef enum {
    DAEMON_CALL,
    DAEMON_DISCOVER,
    DAEMON_HEALTH,
    DAEMON_EVAL,
    DAEMON_TYPECHECK,
    DAEMON_BIND,
    DAEMON_BINDINGS,
    DAEMON_UNBIND
} daemon_method_t;

typedef struct daemon_request_s {
    char* id;
    daemon_method_t method;
    char* command;
    char* args_json;
    char* expr;
    char* name;
} daemon_request_t;

typedef struct daemon_response_s {
    char* id;
    bool success;
    char* result_json;
    char* error;
} daemon_response_t;

// -- HTTP types --

typedef enum {
    HTTP_GET,
    HTTP_POST,
    HTTP_DELETE,
    HTTP_OPTIONS
} http_method_t;

typedef struct http_request_s {
    http_method_t method;
    char path[256];
    char* body;
    size_t body_len;
} http_request_t;

// -- Router types --

typedef struct router_program_s {
    char* name;
    char* manifest_path;
    manifest_t* manifest;
    pid_t daemon_pid;
    char daemon_socket[sizeof(((struct sockaddr_un*)0)->sun_path)];
} router_program_t;

typedef struct router_s {
    router_program_t* programs;
    size_t n_programs;
    char* fdb_path;
} router_t;

// ========================================================================
// Section 8: Pool types
// ========================================================================

typedef uint8_t* (*pool_dispatch_fn_t)(
    uint32_t mid,
    const uint8_t** args,
    size_t nargs,
    void* ctx
);

typedef enum {
    POOL_THREADS,
    POOL_FORK,
    POOL_SINGLE
} pool_concurrency_t;

typedef struct {
    pool_dispatch_fn_t local_dispatch;
    pool_dispatch_fn_t remote_dispatch;
    void* dispatch_ctx;
    pool_concurrency_t concurrency;
    int initial_workers;
    bool dynamic_scaling;
    void (*post_fork_child)(void* ctx);
} pool_config_t;

typedef struct pool_state_s pool_state_t;

// ========================================================================
// Section 9: Arrow types
// ========================================================================

#ifndef ARROW_C_DATA_INTERFACE
#define ARROW_C_DATA_INTERFACE

struct ArrowSchema {
    const char* format;
    const char* name;
    const char* metadata;
    int64_t flags;
    int64_t n_children;
    struct ArrowSchema** children;
    struct ArrowSchema* dictionary;
    void (*release)(struct ArrowSchema*);
    void* private_data;
};

struct ArrowArray {
    int64_t length;
    int64_t null_count;
    int64_t offset;
    int64_t n_buffers;
    int64_t n_children;
    const void** buffers;
    struct ArrowArray** children;
    struct ArrowArray* dictionary;
    void (*release)(struct ArrowArray*);
    void* private_data;
};

#endif // ARROW_C_DATA_INTERFACE

#define ARROW_SHM_MAGIC    0xA770DA7A
#define ARROW_BUFFER_ALIGN 64
#define ARROW_ALIGN_UP(x)  (((x) + ARROW_BUFFER_ALIGN - 1) & ~((size_t)ARROW_BUFFER_ALIGN - 1))

typedef struct arrow_column_desc {
    morloc_serial_type type;
    uint64_t length;
    uint64_t null_count;
    uint32_t name_offset;
    uint16_t name_length;
    uint64_t data_offset;
    uint64_t data_size;
} arrow_column_desc_t;

typedef struct arrow_shm_header {
    uint32_t magic;
    uint32_t n_columns;
    uint64_t n_rows;
    uint64_t total_size;
} arrow_shm_header_t;

// ========================================================================
// Section 10: Slurm / resource types
// ========================================================================

#define MAX_SLURM_COMMAND_LENGTH 1024

typedef struct resources_s {
    int memory; // in GB
    int time;   // walltime in seconds
    int cpus;
    int gpus;
} resources_t;

// ========================================================================
// Section 11: Function declarations -- Memory / SHM
// ========================================================================

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG);
shm_t* shopen(size_t volume_index, ERRMSG);
bool shclose(ERRMSG);
void shm_set_fallback_dir(const char* dir);
void* shmalloc(size_t size, ERRMSG);
void* shmemcpy(void* src, size_t size, ERRMSG);
bool shfree(absptr_t ptr, ERRMSG);
bool shincref(absptr_t ptr, ERRMSG);
bool shfree_by_schema(absptr_t ptr, const Schema* schema, ERRMSG);
void* shcalloc(size_t nmemb, size_t size, ERRMSG);
void* shrealloc(void* ptr, size_t size, ERRMSG);
size_t total_shm_size(void);
volptr_t rel2vol(relptr_t ptr, ERRMSG);
absptr_t rel2abs(relptr_t ptr, ERRMSG);

// Convenience: resolve a relptr, using base_ptr if available (no SHM lookup needed).
static inline void* resolve_relptr(relptr_t relptr, const void* base_ptr, ERRMSG) {
    if (base_ptr) {
        return (char*)base_ptr + relptr;
    }
    return rel2abs(relptr, errmsg_);
}
relptr_t vol2rel(volptr_t ptr, shm_t* shm);
absptr_t vol2abs(volptr_t ptr, shm_t* shm);
relptr_t abs2rel(absptr_t ptr, ERRMSG);
shm_t* abs2shm(absptr_t ptr, ERRMSG);
block_header_t* abs2blk(void* ptr, ERRMSG);

// ========================================================================
// Section 12: Function declarations -- Schema
// ========================================================================

Schema* parse_schema(const char* schema, ERRMSG);
char* schema_to_string(const Schema* schema);
void* get_ptr(const Schema* schema, ERRMSG);
void free_schema(Schema* schema);
bool schema_is_fixed_width(const Schema* schema);
size_t schema_alignment(const Schema* schema);
size_t calculate_voidstar_size(const void* data, const Schema* schema, ERRMSG);

// Inline helpers used by language extensions (pymorloc.c, rmorloc.c)
#define ALIGN_UP(x, align) (((x) + (align) - 1) & ~((size_t)(align) - 1))

static inline size_t schema_tensor_ndim(const Schema* schema) {
    if (schema == NULL || schema->size == 0) return 0;
    // ndim is stored in offsets[0] for tensor schemas
    return schema->offsets ? schema->offsets[0] : 0;
}

// ========================================================================
// Section 13: Function declarations -- Serialisation (pack/unpack)
// ========================================================================

int pack(const void* mlc, const char* schema_str, char** mpkptr, size_t* mpk_size, ERRMSG);
int pack_with_schema(const void* mlc, const Schema* schema, char** mpkptr, size_t* mpk_size, ERRMSG);
int unpack_with_schema(const char* mpk, size_t mpk_size, const Schema* schema, void** mlcptr, ERRMSG);

// ========================================================================
// Section 14: Function declarations -- Packets
// ========================================================================

morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG);
bool packet_is_ping(const uint8_t* packet, ERRMSG);
bool packet_is_local_call(const uint8_t* packet, ERRMSG);
bool packet_is_remote_call(const uint8_t* packet, ERRMSG);
size_t morloc_packet_size_from_header(const morloc_packet_header_t* header);
size_t morloc_packet_size(const uint8_t* packet, ERRMSG);
uint8_t* return_ping(const uint8_t* packet, ERRMSG);
uint8_t* make_ping_packet(void);
uint8_t* make_standard_data_packet(relptr_t ptr, const Schema* schema);
uint8_t* make_arrow_data_packet(relptr_t ptr, const Schema* schema);
uint8_t* make_mpk_data_packet(const char* mpk_filename, const Schema* schema);
uint8_t* make_data_packet_from_mpk(const char* mpk, size_t mpk_size, const Schema* schema);
int get_data_packet_as_mpk(const uint8_t* packet, const Schema* schema, char** mpk_out, size_t* mpk_size_out, ERRMSG);
char* read_schema_from_packet_meta(const uint8_t* packet, ERRMSG);
uint8_t* make_fail_packet(const char* failure_message);
char* get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG);
uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG);
uint8_t* make_morloc_local_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
uint8_t* make_morloc_remote_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG);
void free_morloc_call(morloc_call_t* call);
int print_morloc_data_packet(const uint8_t* packet, const Schema* schema, ERRMSG);
int flatten_voidstar_to_buffer(const void* data, const Schema* schema, uint8_t** out_buf, size_t* out_size, ERRMSG);
uint8_t* make_data_packet_auto(void* voidstar, relptr_t relptr, const Schema* schema, ERRMSG);
int adjust_voidstar_relptrs(void* data, const Schema* schema, relptr_t base_rel, ERRMSG);
void* read_voidstar_binary(const uint8_t* blob, size_t blob_size, const Schema* schema, ERRMSG);
bool parse_morloc_call_arguments(uint8_t* packet, uint8_t** args, size_t* nargs, ERRMSG);
bool hash_morloc_packet(const uint8_t* packet, const Schema* schema, uint64_t seed, uint64_t* hash, ERRMSG);

// ========================================================================
// Section 15: Function declarations -- Printing / output
// ========================================================================

char* quoted(const char* input);
bool print_voidstar(const void* voidstar, const Schema* schema, ERRMSG);
bool pretty_print_voidstar(const void* voidstar, const Schema* schema, ERRMSG);
bool print_arrow_as_json(const void* data, ERRMSG);
bool print_arrow_as_table(const void* data, ERRMSG);
bool print_hex_dump(const uint8_t* data, size_t size, ERRMSG);
char* voidstar_to_json_string(const void* voidstar, const Schema* schema, ERRMSG);

// ========================================================================
// Section 16: Function declarations -- Daemon / socket communication
// ========================================================================

void close_socket(int socket_id);
void close_daemon(language_daemon_t** daemon_ptr);
language_daemon_t* start_daemon(
    const char* socket_path, const char* tmpdir,
    const char* shm_basename, size_t shm_default_size, ERRMSG);
uint8_t* stream_from_client_wait(int client_fd, int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* stream_from_client(int client_fd, ERRMSG);
uint8_t* send_and_receive_over_socket_wait(
    const char* socket_path, const uint8_t* packet,
    int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* send_and_receive_over_socket(const char* socket_path, const uint8_t* packet, ERRMSG);
size_t send_packet_to_foreign_server(int client_fd, uint8_t* packet, ERRMSG);
int wait_for_client_with_timeout(language_daemon_t* daemon, int timeout_us, ERRMSG);
int wait_for_client(language_daemon_t* daemon, ERRMSG);

// Daemon event loop and dispatch.
void daemon_run(daemon_config_t* config, manifest_t* manifest,
                morloc_socket_t* sockets, size_t n_pools,
                const char* shm_basename);
daemon_response_t* daemon_dispatch(manifest_t* manifest,
                                   daemon_request_t* request,
                                   morloc_socket_t* sockets,
                                   const char* shm_basename);
daemon_request_t* daemon_parse_request(const char* json, size_t len, ERRMSG);
daemon_response_t* daemon_parse_response(const char* json, size_t len, ERRMSG);
char* daemon_serialize_response(daemon_response_t* response, size_t* out_len);
char* daemon_build_discovery(manifest_t* manifest);
void daemon_set_eval_timeout(int timeout_sec);
void daemon_free_request(daemon_request_t* req);
void daemon_free_response(daemon_response_t* resp);

// Binding store (public types only; internal hash table functions are Rust-side).
binding_store_t* binding_store_init(const char* base_dir);
void binding_store_free(binding_store_t* store);

// ========================================================================
// Section 17: Function declarations -- HTTP
// ========================================================================

http_request_t* http_parse_request(int fd, ERRMSG);
bool http_write_response(int fd, int status, const char* content_type,
                         const char* body, size_t body_len);
daemon_request_t* http_to_daemon_request(http_request_t* req, ERRMSG);
void http_free_request(http_request_t* req);

// ========================================================================
// Section 18: Function declarations -- Router
// ========================================================================

router_t* router_init(const char* fdb_path, ERRMSG);
void router_run(daemon_config_t* config, router_t* router);
bool router_start_program(router_program_t* prog, ERRMSG);
daemon_response_t* router_forward(router_t* router, const char* program,
                                  daemon_request_t* request, ERRMSG);
char* router_build_discovery(router_t* router);
void router_free(router_t* router);

// ========================================================================
// Section 19: Function declarations -- Pool
// ========================================================================

int pool_main(int argc, char** argv, pool_config_t* config);
uint8_t* pool_dispatch_packet(
    const uint8_t* packet,
    pool_dispatch_fn_t local_dispatch,
    pool_dispatch_fn_t remote_dispatch,
    void* ctx);
void pool_mark_busy(void);
void pool_mark_idle(void);

// ========================================================================
// Section 20: Function declarations -- Arrow
// ========================================================================

size_t arrow_element_size(morloc_serial_type type);
const char* arrow_format_string(morloc_serial_type type);
morloc_serial_type arrow_format_to_type(const char* format);
relptr_t arrow_to_shm(const struct ArrowArray* array, const struct ArrowSchema* schema, ERRMSG);
int arrow_validate(const arrow_shm_header_t* header, const Schema* schema, ERRMSG);
const void* arrow_column_data(const arrow_shm_header_t* header, uint32_t col_index);
const arrow_column_desc_t* arrow_column_desc(const arrow_shm_header_t* header, uint32_t col_index);
const char* arrow_column_name(const arrow_shm_header_t* header, uint32_t col_index);
int arrow_from_shm(const arrow_shm_header_t* header,
                   struct ArrowSchema* out_schema,
                   struct ArrowArray* out_array, ERRMSG);

// ========================================================================
// Section 21: Function declarations -- Cache
// ========================================================================

char* put_cache_packet(const uint8_t* voidstar, const Schema* schema, uint64_t key, const char* cache_path, ERRMSG);
uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
char* check_cache_packet(uint64_t key, const char* cache_path, ERRMSG);

// ========================================================================
// Section 22: Function declarations -- CLI / argument parsing
// ========================================================================

argument_t* initialize_positional(char* value);
argument_t* initialize_unrolled(size_t size, char* default_value, char** fields, char** default_fields);
void free_argument_t(argument_t* arg);
uint8_t* parse_cli_data_argument(uint8_t* dest, const argument_t* arg, const Schema* schema, ERRMSG);
uint8_t* make_call_packet_from_cli(
    uint8_t* dest, uint32_t mid,
    argument_t** args, char** arg_schema_strs, ERRMSG);
void* load_morloc_data_file(const char* path, uint8_t* data, size_t data_size, const Schema* schema, ERRMSG);

// ========================================================================
// Section 23: Function declarations -- Expression evaluation
// ========================================================================

morloc_expression_t* make_morloc_bound_var(const char* schema_str, char* varname, ERRMSG);
morloc_expression_t* make_morloc_literal(const char* schema_str, primitive_t lit, ERRMSG);
morloc_expression_t* make_morloc_pattern(const char* schema_str, morloc_pattern_t* pattern, ERRMSG);
morloc_pattern_t* make_morloc_pattern_end(void);
absptr_t morloc_eval(
    morloc_expression_t* expr, Schema* return_schema,
    uint8_t** arg_voidstar, Schema** arg_schemas, size_t nargs, ERRMSG);

// ========================================================================
// Section 24: Function declarations -- Manifest
// ========================================================================

manifest_t* parse_manifest(const char* text, ERRMSG);
manifest_t* read_manifest(const char* path, ERRMSG);
void free_manifest(manifest_t* manifest);
morloc_expression_t* build_manifest_expr(const char* json_str, ERRMSG);
char* manifest_to_discovery_json(const manifest_t* manifest);

// ========================================================================
// Section 25: Function declarations -- Intrinsics
// ========================================================================

int mlc_save(const absptr_t data, const Schema* schema, const char* path, ERRMSG);
int mlc_save_json(const absptr_t data, const Schema* schema, const char* path, ERRMSG);
int mlc_save_voidstar(const absptr_t data, const Schema* schema, const char* path, ERRMSG);
void* mlc_load(const char* path, const Schema* schema, ERRMSG);
char* mlc_hash(const absptr_t data, const Schema* schema, ERRMSG);
char* mlc_show(const absptr_t data, const Schema* schema, ERRMSG);
void* mlc_read(const char* json_str, const Schema* schema, ERRMSG);
relptr_t write_voidstar_binary(int fd, const void* data, const Schema* schema, ERRMSG);

// ========================================================================
// Section 26: Function declarations -- Slurm
// ========================================================================

size_t parse_slurm_time(const char* time_str, ERRMSG);
char* write_slurm_time(int seconds);
bool slurm_job_is_complete(uint32_t job_id);
uint32_t submit_morloc_slurm_job(
    const char* nexus_path,
    const char* socket_basename,
    const char* call_packet_filename,
    const char* result_cache_filename,
    const char* output_filename,
    const char* error_filename,
    const resources_t* resources,
    ERRMSG);
uint8_t* remote_call(
    int midx,
    const char* socket_basename,
    const char* cache_path,
    const resources_t* resources,
    const uint8_t** arg_packets,
    size_t nargs,
    ERRMSG);

// ========================================================================
// Section 27: Function declarations -- Utility
// ========================================================================

void hex(const void* ptr, size_t size);
bool file_exists(const char* filename);
int mkdir_p(const char* path, ERRMSG);
void delete_directory(const char* path);
bool has_suffix(const char* x, const char* suffix);
int write_atomic(const char* filename, const uint8_t* data, size_t size, ERRMSG);
int write_binary_fd(int fd, const char* buf, size_t count, ERRMSG);
int print_binary(const char* buf, size_t count, ERRMSG);
uint8_t* read_binary_fd(FILE* file, size_t* file_size, ERRMSG);
uint8_t* read_binary_file(const char* filename, size_t* file_size, ERRMSG);

// ========================================================================
// Section 28: Function declarations -- Hashing
// ========================================================================

uint64_t morloc_xxh64(const void* input, size_t length, uint64_t seed);

// ========================================================================
// Section 29: Function declarations -- JSON reader
// ========================================================================

uint8_t* read_json_with_schema(uint8_t* voidstar, char* json_data, const Schema* schema, ERRMSG);

#ifdef __cplusplus
}
#endif

#endif // __MORLOC_H__
