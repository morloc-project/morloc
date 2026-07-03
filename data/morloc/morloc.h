// morloc.h -- C ABI contract for libmorloc.so
//
// This is the single public header for consumers of the morloc runtime library.
// It defines all types and function declarations exported by libmorloc.so.

#ifndef __MORLOC_H__
#define __MORLOC_H__

// Atomic includes must sit outside any `extern "C"` block because the
// C++ <atomic> header pulls in <type_traits> et al., which use C++
// templates. We define the platform-neutral macros here, then open
// the extern "C" block once we are past the headers.
#if defined(__cplusplus)
#  include <atomic>
#  define MORLOC_ATOMIC_PTR(T)   std::atomic<T*>
#  define MORLOC_ATOMIC_SIZE     std::atomic<size_t>
#  define MORLOC_ATOMIC_LOAD_ACQ(x)  ((x).load(std::memory_order_acquire))
#  define MORLOC_ATOMIC_LOAD_RLX(x)  ((x).load(std::memory_order_relaxed))
#else
#  include <stdatomic.h>
#  define MORLOC_ATOMIC_PTR(T)   _Atomic(T*)
#  define MORLOC_ATOMIC_SIZE     _Atomic(size_t)
#  define MORLOC_ATOMIC_LOAD_ACQ(x)  atomic_load_explicit(&(x), memory_order_acquire)
#  define MORLOC_ATOMIC_LOAD_RLX(x)  atomic_load_explicit(&(x), memory_order_relaxed)
#endif

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

#define MAX_VOLUME_NUMBER 32768

// Indexed relptr encoding: bit 63 = sentinel, bits 62..48 = 15-bit
// volume index, bits 47..0 = 48-bit offset within the volume's data
// region. Mirrors morloc-runtime-types::shm_types; kept here so the
// C/C++ bridges can do per-relptr arithmetic without crossing FFI.
#define RELPTR_VOL_IDX_SHIFT 48
#define RELPTR_VOL_IDX_MASK  ((uint64_t)0x7FFF)
#define RELPTR_OFFSET_MASK   ((uint64_t)0x0000FFFFFFFFFFFF)
#define RELPTR_SENTINEL_MASK ((uint64_t)0x8000000000000000)

static inline size_t relptr_vol_idx(relptr_t p) {
    return (size_t)(((uint64_t)p >> RELPTR_VOL_IDX_SHIFT) & RELPTR_VOL_IDX_MASK);
}

static inline size_t relptr_offset_bits(relptr_t p) {
    return (size_t)((uint64_t)p & RELPTR_OFFSET_MASK);
}

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

// Explicit values pin the wire-format ABI. Slot 12 is reserved (formerly
// MORLOC_TENSOR; tensors now serialize as Packable tuples of (dims, data)).
// Subsequent slots keep their original values so the wire format is
// backward-compatible at the byte level.
typedef enum {
    MORLOC_NIL      = 0,
    MORLOC_BOOL     = 1,
    MORLOC_SINT8    = 2,
    MORLOC_SINT16   = 3,
    MORLOC_SINT32   = 4,
    MORLOC_SINT64   = 5,
    MORLOC_UINT8    = 6,
    MORLOC_UINT16   = 7,
    MORLOC_UINT32   = 8,
    MORLOC_UINT64   = 9,
    MORLOC_FLOAT32  = 10,
    MORLOC_FLOAT64  = 11,
    /* slot 12 reserved (formerly tensor) */
    MORLOC_STRING   = 13,
    MORLOC_ARRAY    = 14,
    MORLOC_TUPLE    = 15,
    MORLOC_MAP      = 16,
    MORLOC_OPTIONAL = 17,
    MORLOC_INT      = 18, // variable-width integer (Array of uint64_t limbs, two's complement)
    MORLOC_TABLE    = 19, // Arrow IPC primitive. Schema columns (if any) are
                          // open constraints on the buffer's actual schema; the
                          // binary layout is fully described by the Arrow
                          // buffer itself, not by the morloc Schema. Wire form
                          // is `T` (no declared columns) or `T:K<entries>`.
    MORLOC_RECUR    = 20, // Back-reference to a named schema declared by
                          // `&<klen><name>` earlier on the path. Carries the
                          // referenced name in `Schema.name`; structurally
                          // terminates descent at the back-ref point. Wire form
                          // is `^<klen><name>`.
    MORLOC_IFILE    = 21, // Random-access stream-file handle (read-only).
    MORLOC_OSTREAM  = 22, // Sequential stream-file writer handle.
    MORLOC_ISTREAM  = 23  // Sequential stream-file reader handle.
    // Stream-handle types (`F`/`O`/`I`) share a 16-byte tagged-union wire
    // form. The schema code selects the morloc-level type; the tag byte
    // (byte 0 of the field) picks the encoding: `TAG_PATH` (0) means the
    // payload is a relptr to a path suballoc `{size: u64, bytes...}`,
    // `TAG_HANDLE` (1) means the payload is a bare slot id valid only in
    // the sender+receiver's shared SHM registry (intra-nexus). See
    // `morloc_runtime_types::stream_handle` for the layout. The in-
    // language native form is `uint64_t` (the local slot id) regardless
    // of tag; sender and receiver each own independent fds, mmaps, and
    // arena-tracked close lifetimes.
} morloc_serial_type;

// Single-character schema encoding tokens.
#define SCHEMA_NIL      'z'
#define SCHEMA_BOOL     'b'
#define SCHEMA_SINT     'i'
#define SCHEMA_UINT     'u'
#define SCHEMA_FLOAT    'f'
#define SCHEMA_STRING   's'
#define SCHEMA_ARRAY    'a'
#define SCHEMA_TUPLE    't'
#define SCHEMA_MAP      'm'
#define SCHEMA_OPTIONAL '?'
#define SCHEMA_INT      'j'
#define SCHEMA_TABLE    'T'
#define SCHEMA_IFILE    'F'
#define SCHEMA_OSTREAM  'O'
#define SCHEMA_ISTREAM  'I'

// Schema: recursive type descriptor used for serialisation/deserialisation.
//
// `name` carries either a named-schema declaration (on the outer node of
// a `&<klen><name>X` wire form, representing a recursive record like
// `Tree`) or a back-reference target (on every `MORLOC_RECUR` node).
// NULL for all non-recursive schemas. Appended at the struct's tail so
// adding the field does not move any pre-existing offsets.
struct Schema;
typedef struct Schema {
    morloc_serial_type type;
    size_t size;       // number of parameters
    size_t width;      // bytes per element when stored in a fixed-width array
    size_t* offsets;   // field offsets (tuples)
    char* hint;
    struct Schema** parameters;
    char** keys;       // field names (records only)
    char* name;        // recursive-schema declaration / back-ref name (or NULL)
} Schema;

// Variable-length array in voidstar representation.
typedef struct Array {
    size_t size;
    relptr_t data;
} Array;

// ========================================================================
// Section 4: Packet types
// ========================================================================

#define MORLOC_PACKET_MAGIC 0x0707f86d

// Packet type discriminator.
typedef uint8_t command_type_t;
#define PACKET_TYPE_DATA   ((command_type_t)0)
#define PACKET_TYPE_CALL   ((command_type_t)1)
#define PACKET_TYPE_PING   ((command_type_t)2)
#define PACKET_TYPE_STREAM ((command_type_t)3)
#define PACKET_TYPE_FOOTER ((command_type_t)4)

// Stream packet length-field sentinel. Streams are append-only and have
// no global content length; the header's `length` field is always set
// to (uint64_t)-1. Future versions may interpret a non-sentinel value
// as a declared maximum stream length.
#define STREAM_LENGTH_SENTINEL ((uint64_t)0xFFFFFFFFFFFFFFFFULL)

// Stream EOF tail: 8 bytes at end-of-file when a stream has a footer.
// Layout: [footer_length: u32 LE][footer_magic: 4 bytes].
// STREAM_TAIL_MAGIC is MORLOC_PACKET_MAGIC (0x0707f86d) with its bytes
// reversed: { 0x07, 0x07, 0xf8, 0x6d }. Since the same bytes in LE
// (6d f8 07 07) are the start of every packet header, the reversed
// form cannot collide with a packet-header start byte sequence.
// A reader checks the last 8 bytes; magic match -> footer present at
// (file_size - 8 - footer_length). The reader then re-validates the
// candidate footer's own header (PACKET_MAGIC + PACKET_TYPE_FOOTER)
// before trusting it.
#define STREAM_TAIL_SIZE 8

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
#define PACKET_COMPRESSION_ZSTD 0x01
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

// Default inline threshold: voidstar data <= this size is embedded in
// the packet payload (PACKET_SOURCE_MESG); larger payloads route to
// shared memory (PACKET_SOURCE_RPTR) or, when SHM is disabled, to a
// temp file (PACKET_SOURCE_FILE). This is only the compile-time
// default; the effective value at runtime is tunable per-program via
// `morloc make --inline-size`, propagated through the manifest and
// the MORLOC_INLINE_SIZE env var, and finally settable directly via
// the morloc_set_inline_threshold() FFI below.
#define MORLOC_INLINE_THRESHOLD (64 * 1024)

// Override the inline threshold at runtime. Values < 0 are clamped to
// 0 (never inline). Returns no status; idempotent.
void morloc_set_inline_threshold(int64_t bytes);

// Read the live inline threshold (in bytes).
uint64_t morloc_get_inline_threshold(void);

// Toggle shared memory at runtime. When disabled, payloads above the
// inline threshold are written to a temp file (PACKET_SOURCE_FILE)
// instead of shared memory (PACKET_SOURCE_RPTR). Set automatically by
// the nexus on startup when the manifest carries `no_shm: true`
// (from `morloc make --no-shm`); pool processes inherit the
// MORLOC_NO_SHM env var on spawn.
void morloc_set_shm_enabled(bool enabled);

// Read the live SHM-enabled flag.
bool morloc_get_shm_enabled(void);

// Override the directory where file-packet intermediates land when
// SHM is disabled. NULL or "" clears the override (libmorloc falls
// back to $TMPDIR or /tmp). The directory is created lazily on
// first write if it does not exist. Set automatically by the nexus
// on startup when the manifest carries a `tmpdir` (from
// `morloc make --tmpdir`); pool processes inherit MORLOC_TMPDIR on
// spawn.
//
// Persistence: when a tmpdir is set (non-NULL, non-empty), file
// packets are NOT auto-deleted at end-of-eval. The caller has
// explicitly named the directory and is responsible for cleanup.
// With no override (default), files are unlinked when the
// surrounding eval scope ends via eval_arena.
void morloc_set_tmpdir(const char* path);

// ========================================================================
// Log emission for `log: true` labeled manifolds
// ========================================================================

// Allocate a fresh call id for pairing start / pass / fail log lines.
// The returned u64 is opaque to the pool; internally morloc_log_emit
// renders it as "{pid}:{counter}" for the {id} placeholder.
uint64_t morloc_log_next_id(void);

// Emit one formatted log line to stderr. `tmpl` is a null-terminated
// pre-rendered template (the morloc compiler has already substituted
// static placeholders like {name}, {module}, and the ANSI color codes
// from {c:red}). This function fills in the runtime placeholders
// ({date}, {runtime}, {id}), strips ANSI CSI sequences when stderr is
// not a TTY (or when NO_COLOR is set), and writes the line + newline
// atomically. `runtime_seconds` is the duration to substitute into
// {runtime}; pass 0.0 for start events. `call_id` should come from
// morloc_log_next_id and be reused across the start/pass/fail trio
// for one invocation. `group` is the label group (e.g. "a" for an
// a:foo reference); when MORLOC_RUN_DIR is set, the color-stripped
// line is also appended to $MORLOC_RUN_DIR/<group>/log. NULL or empty
// `group` skips the tee. NULL `tmpl` is a no-op.
void morloc_log_emit(
    const char* tmpl,
    const char* group,
    double runtime_seconds,
    uint64_t call_id
);

// ========================================================================
// Per-run workdir lifecycle
// ========================================================================

// Force resolution of the run id and republish the
// MORLOC_RUN_DIR / MORLOC_RUN_BASE / MORLOC_RUN_PARENT_PID env vars
// for child processes (pools, nested morloc programs) to inherit.
// Called from the nexus at startup. The actual filesystem directory
// is NOT created here -- materialization is lazy on first writer.
void morloc_run_init(void);

// Write summary.json (if the run directory has been materialized) and
// flush any per-label log tee handles. Called from the nexus's
// clean_exit after pool teardown so any final pool-side log writes
// land in the per-label files before the handles close.
void morloc_run_finalize(int exit_code);

// Metadata sub-header in packet metadata sections.
#define MORLOC_METADATA_TYPE_SCHEMA_STRING   0x01
#define MORLOC_METADATA_TYPE_XXHASH          0x02
#define MORLOC_METADATA_TYPE_VOL_INDEX       0x03
#define MORLOC_METADATA_TYPE_FRAME_INDEX     0x04
#define MORLOC_METADATA_TYPE_SUBPACKET_INDEX 0x05
#define MORLOC_METADATA_TYPE_STREAM_DIAG     0x06
#define MORLOC_METADATA_TYPE_FOOTER_FINAL    0x07
#define MORLOC_METADATA_HEADER_MAGIC         "mmh"

typedef struct __attribute__((packed)) morloc_metadata_header_s {
    char magic[3];
    uint8_t type;
    uint32_t size;
} morloc_metadata_header_t;

// Handle-kind discriminants for mlc_open / mlc_close. Must agree with
// morloc-runtime-types::packet::MLC_KIND_*.
#define MLC_KIND_IFILE   0
#define MLC_KIND_ISTREAM 1
#define MLC_KIND_OSTREAM 2

// Upper bound on the file-path payload carried by a single IFile
// handle in voidstar wire form. POSIX PATH_MAX on Linux is 4096; we
// use that here as the conservative ceiling for buffer sizing in the
// per-language IFile wrappers and in nexus eval.
#define MORLOC_IFILE_PATH_MAX 4096

// Fixed-size diagnostic block carried inside every stream footer. The
// temp footer's whole purpose is this block. Tooling like
// `morloc-nexus file` reads this directly from a STREAM_PACKET file's
// EOF to produce a writer status report. See morloc-runtime-types
// (packet.rs) for the canonical definition and the byte-counter
// semantics (PAYLOAD-only totals so the ratio is clean).
#define MORLOC_STREAM_DIAG_VERSION       1
#define MORLOC_STREAM_DIAG_TAIL_MAX      8
#define MORLOC_STREAM_TEMP_FOOTER_BYTES 208  // 32 (hdr) + 8 (meta hdr) + 160 (diag) + 8 (tail)

typedef struct __attribute__((packed)) morloc_stream_diag_s {
    // Header lane: four u32s, two u64-aligned slots (16 B)
    uint32_t diag_version;
    uint32_t writer_pid;
    uint32_t n_oversize_subpackets;
    uint32_t tail_len;

    // Reserved (8 B) for future-extensible fields
    uint64_t _reserved;

    // Identification + running totals (72 B = 9 * u64)
    uint64_t writer_start_time;            // unix-epoch microseconds at @open
    uint64_t subpacket_count;
    uint64_t element_count;
    uint64_t bytes_uncompressed_total;     // PAYLOAD-only
    uint64_t bytes_compressed_total;       // PAYLOAD-only
    uint64_t largest_packet_uncompressed;
    uint64_t largest_packet_idx;
    uint64_t first_flush_time;
    uint64_t last_flush_time;

    // Tail window (MORLOC_STREAM_DIAG_TAIL_MAX * 8 = 64 B). Only
    // tail[0..tail_len] is meaningful.
    uint64_t tail[MORLOC_STREAM_DIAG_TAIL_MAX];
} morloc_stream_diag_t;

#if !defined(__cplusplus)
_Static_assert(sizeof(morloc_stream_diag_t) == 160,
               "morloc_stream_diag_t must be 160 bytes; struct/wire mismatch");
#else
static_assert(sizeof(morloc_stream_diag_t) == 160,
              "morloc_stream_diag_t must be 160 bytes; struct/wire mismatch");
#endif

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

// Length-aware string literal payload. Used wherever the nexus expression
// tree carries a morloc Str: the `data` buffer is NOT NUL-terminated and may
// contain interior NUL bytes. The companion `s` member of primitive_t below
// is reserved for BigInt decimal literals, which are always plain C-strings.
typedef struct morloc_string_s {
    char*  data;
    size_t size;
} morloc_string_t;

typedef union primitive_u {
    char*            s;    // BigInt "j" decimal-string only (no interior NUL)
    morloc_string_t* str;  // Str literals (NUL-safe)
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
        // Length-aware literal pieces for string interpolation; aliases
        // morloc_expression_t.expr.interpolation on the wrapped Fmt node.
        morloc_string_t** fmt;
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
        // Length-aware array of literal pieces for string interpolation
        // (Fmt expressions). Each piece may contain interior NULs.
        morloc_string_t** interpolation;
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
    char** exec;             // NULL-terminated array
    char* socket;            // socket basename
    char* metadata_json;     // reserved
    bool  allow_string_null; // whether the pool's language can carry interior-NUL Str
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
    char* metadata_json;        // reserved
    // When true, skip the runtime NUL-in-Str scan at cross-pool
    // boundaries; set via `morloc make --unsafe-skip-null-check`.
    bool unsafe_skip_null_check;
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

// Port sentinel convention:
//   tcp_port / http_port == -1  -> listener not configured
//   tcp_port / http_port ==  0  -> bind ephemeral (OS picks the port);
//                                  daemon_run reads it back with
//                                  getsockname() and emits an stderr
//                                  ready line plus an entry in the
//                                  optional port_file_path JSON.
//   0..=65535 otherwise         -> bind that specific port.
//
// port_file_path is optional. When non-NULL, daemon_run writes a JSON
// document of shape {"http":N|null,"tcp":N|null,"unix":"PATH"|null}
// atomically (tmp + rename) after all listeners are bound. This is the
// race-free discovery channel for orchestrators spawning many daemons
// in parallel.
typedef struct daemon_config_s {
    const char* unix_socket_path;
    int tcp_port;
    int http_port;
    const char* port_file_path;
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

// Error classification for daemon failures. On success, error_kind is
// DAEMON_ERROR_OK (0). On failure, it drives HTTP status mapping in
// handle_http_connection (BAD_REQUEST -> 400, NOT_FOUND -> 404,
// TIMEOUT -> 408, RECOVERING -> 503, INTERNAL -> 500). Unix/TCP clients
// read `success` and the {"status":"error","error":"..."} envelope and
// do not see error_kind on the wire today.
#define DAEMON_ERROR_OK          0
#define DAEMON_ERROR_BAD_REQUEST 1
#define DAEMON_ERROR_NOT_FOUND   2
#define DAEMON_ERROR_TIMEOUT     3
#define DAEMON_ERROR_RECOVERING  4
#define DAEMON_ERROR_INTERNAL    5

typedef struct daemon_response_s {
    char* id;
    bool success;
    int error_kind;
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
void* shcalloc(size_t nmemb, size_t size, ERRMSG);
void* shrealloc(void* ptr, size_t size, ERRMSG);
size_t total_shm_size(void);
volptr_t rel2vol(relptr_t ptr, ERRMSG);
absptr_t rel2abs(relptr_t ptr, ERRMSG);

// ── Lock-free per-process volume base table ─────────────────────────────────
//
// libmorloc.so publishes a base+size entry for every SHM volume it has
// mapped into this process. `resolve_relptr` below reads from the
// table inline, replacing what used to be a mutex-guarded FFI call
// (~100 ns) with an Acquire-load + branch + add (~5 ns). On a miss --
// volume not yet mapped in this process -- it falls through to the
// FFI `rel2abs` which lazily opens the segment and publishes the
// entry. Publication and withdrawal happen inside libmorloc.so's
// `shinit` / `shopen_diag` / `shclose`.
//
// Layout is part of the libmorloc.so ABI:
struct morloc_vol_entry {
    MORLOC_ATOMIC_PTR(void) data_base;   // 0 if slot empty in this process
    MORLOC_ATOMIC_SIZE      data_size;   // valid when data_base != 0
};
extern struct morloc_vol_entry MORLOC_VOL_TABLE[MAX_VOLUME_NUMBER];

#if !defined(__cplusplus)
_Static_assert(sizeof(MORLOC_ATOMIC_PTR(void))   == sizeof(void*),
               "atomic pointer layout mismatch");
_Static_assert(sizeof(MORLOC_ATOMIC_SIZE)        == sizeof(size_t),
               "atomic size_t layout mismatch");
_Static_assert(sizeof(struct morloc_vol_entry)   == 2 * sizeof(void*),
               "morloc_vol_entry layout mismatch");
#endif

// Resolve a relptr. Three paths, in priority order:
//
//   1. `base_ptr` is non-null  -- inline MESG+VOIDSTAR data: the relptr
//      is a buffer-relative offset; strip the vol_idx bits (they are
//      zero in practice for inline producers, but masking is harmless
//      either way) and add to base_ptr.
//
//   2. Lock-free table hit     -- the volume the relptr targets is
//      mapped in this process. Acquire-load the entry, bounds-check
//      the offset, return data_base + offset. No FFI, no mutex.
//
//   3. Lock-free table miss    -- volume not yet mapped here. Fall
//      through to the FFI `rel2abs`, which lazily opens the segment
//      and publishes the entry. Subsequent calls hit path (2).
static inline void* resolve_relptr(relptr_t relptr, const void* base_ptr, ERRMSG) {
    if (base_ptr) {
        return (char*)base_ptr + relptr_offset_bits(relptr);
    }
    size_t vol = relptr_vol_idx(relptr);
    void* data_base = MORLOC_ATOMIC_LOAD_ACQ(MORLOC_VOL_TABLE[vol].data_base);
    if (data_base) {
        size_t off = relptr_offset_bits(relptr);
        size_t data_size = MORLOC_ATOMIC_LOAD_RLX(MORLOC_VOL_TABLE[vol].data_size);
        if (off < data_size) {
            return (char*)data_base + off;
        }
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

// SIMD/BLAS-friendly alignment for Array data buffers when the element type is
// a primitive numeric. Fixed 64-byte constant in the wire format spec --
// covers SSE/AVX/AVX-512 + cache lines on every common architecture, and the
// per-array slack overhead (<= 63 bytes) is negligible for large arrays.
#define MORLOC_ARRAY_DATA_ALIGN 64

static inline bool is_primitive_numeric(const Schema* schema) {
    if (schema == NULL) return false;
    switch (schema->type) {
        case MORLOC_SINT8: case MORLOC_SINT16: case MORLOC_SINT32: case MORLOC_SINT64:
        case MORLOC_UINT8: case MORLOC_UINT16: case MORLOC_UINT32: case MORLOC_UINT64:
        case MORLOC_FLOAT32: case MORLOC_FLOAT64:
            return true;
        default:
            return false;
    }
}

// Alignment for an Array's element data buffer in SHM. For primitive numerics
// we bump to MORLOC_ARRAY_DATA_ALIGN (SIMD/BLAS); otherwise the element's
// natural alignment.
static inline size_t array_data_alignment(const Schema* elem) {
    size_t natural = schema_alignment(elem);
    if (is_primitive_numeric(elem)) {
        return MORLOC_ARRAY_DATA_ALIGN > natural ? MORLOC_ARRAY_DATA_ALIGN : natural;
    }
    return natural;
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
uint8_t* make_data_indirection_packet(const char* dat_filename, const Schema* schema);
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
// Returns: 0 = ok, 1 = error (see errmsg), 2 = downstream pipe closed
// (BrokenPipe on write). On 2, errmsg is NOT set; caller should exit with
// 141 (128 + SIGPIPE) to match conventional CLI behavior.
int32_t print_voidstar(const void* voidstar, const Schema* schema, bool keep_null, ERRMSG);
int32_t pretty_print_voidstar(const void* voidstar, const Schema* schema, bool keep_null, ERRMSG);
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
// Like http_write_response, but appends `extra_headers` (one or more
// `Name: value\r\n` lines, NUL-terminated, may be NULL) to the response
// header block. Used for `Retry-After: 1` on 503 responses.
bool http_write_response_ex(int fd, int status, const char* content_type,
                            const char* body, size_t body_len,
                            const char* extra_headers);
// `error_kind_out` (nullable) receives DAEMON_ERROR_BAD_REQUEST for
// malformed body / missing fields and DAEMON_ERROR_NOT_FOUND for an
// unknown route.
daemon_request_t* http_to_daemon_request(http_request_t* req, ERRMSG, int* error_kind_out);
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

// Per-pool source fingerprint, parsed once from MORLOC_POOL_HASH at
// first call and cached. Returns 0 when the env var is absent. The
// pool's cache wrap mixes this into every cache key so editing the
// morloc source (or any declared hash-include file) invalidates every
// stale entry from this pool.
uint64_t morloc_pool_hash(void);

// Resolve and create the per-label cache directory beneath the unified
// base ($MORLOC_CACHE_BASE, $XDG_CACHE_HOME/morloc/cache, or
// ~/.cache/morloc/cache by default). A NULL or empty `label` resolves
// to a synthetic `_unlabeled` subdirectory. Returns a heap-allocated
// path string the caller frees via libc::free, NULL on failure.
char* morloc_cache_path(const char* label, ERRMSG);

// Counters consumed by the run summary (Stage 4). Each event is
// independent: a `record_miss` followed by a successful compute and
// store fires `record_store` as well. NULL out-pointers in
// `morloc_cache_stats` are ignored.
void morloc_cache_record_hit(void);
void morloc_cache_record_miss(void);
void morloc_cache_record_store(void);
void morloc_cache_stats(uint64_t* hits_out, uint64_t* misses_out, uint64_t* stores_out);

// Compute the cache key from a manifold id and N byte slices, one per
// argument's content (resolved through the schema so SHM-backed packets
// hash their actual values, NEVER raw relptr bits). The seed chain
// mixes in pool_hash (read once from MORLOC_POOL_HASH) so source edits
// invalidate cache entries. Each `arg_schemas[i]` must be a non-null,
// well-formed schema string for `arg_packets[i]`; on any null or
// unparseable schema the function returns 0 and sets *errmsg.
uint64_t morloc_cache_key_compute(
    uint32_t midx,
    const uint8_t* const* arg_packets,
    const char* const* arg_schemas,
    size_t n_args,
    ERRMSG
);

// Look up a cached result packet under <label>/<key>.packet. Returns
// heap bytes (caller frees with free()) and writes the byte length to
// *size_out on hit. NULL on miss (not an error) or on I/O failure
// (errmsg set). Does NOT call morloc_cache_record_hit() -- the wrap
// records hits/misses explicitly so the dispatch-loop and call_cached
// paths attribute their own results.
uint8_t* morloc_cache_lookup(uint64_t key, const char* label, size_t* size_out, ERRMSG);

// Store the value carried by a packet under <label>/<key>.packet,
// materializing the packet to a self-contained inline-msgpack form
// before writing so SHM-backed (PACKET_SOURCE_RPTR) and file-backed
// (PACKET_SOURCE_FILE) payloads survive across processes. `schema_str`
// must describe the packet payload; on schema-parse or
// materialization failure the store fails and *errmsg is set. Atomic
// write. Returns true on success. Does NOT call
// morloc_cache_record_store().
bool morloc_cache_store(
    uint64_t key,
    const char* label,
    const uint8_t* data,
    size_t size,
    const char* schema_str,
    ERRMSG
);

// ========================================================================
// Section 22: Function declarations -- CLI / argument parsing
// ========================================================================

argument_t* initialize_positional(char* value);
argument_t* initialize_unrolled(size_t size, char* default_value, char** fields, char** default_fields);
void free_argument_t(argument_t* arg);
uint8_t* parse_cli_data_argument(uint8_t* dest, const argument_t* arg, const Schema* schema, ERRMSG);
// Variadic-list assembly entry point (matches `--' many: true` in the
// nexus manifest). Parses N CLI argument_t's into a single packet
// wrapping a morloc list whose element schema is the list schema's
// inner type. Each element goes through the same source classifier
// (inline JSON / file / stdin) as parse_cli_data_argument, so binary
// payloads (mpack, morloc voidstar, arrow IPC) are accepted per
// element without JSON conversion.
uint8_t* parse_cli_data_argument_list(
    uint8_t* dest, const argument_t* const* args, size_t n,
    const Schema* list_schema, ERRMSG);
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

int mlc_save(const absptr_t data, const Schema* schema, uint8_t level, const char* path, ERRMSG);
int mlc_save_json(const absptr_t data, const Schema* schema, uint8_t level, const char* path, ERRMSG);
// @save voidstar: produces a morloc data packet. When level > 0 the
// packet's payload is zstd-compressed and the header carries
// PACKET_COMPRESSION_ZSTD; level == 0 writes uncompressed (legacy shape).
int mlc_save_voidstar(const absptr_t data, const Schema* schema, uint8_t level, const char* path, ERRMSG);
void* mlc_load(const char* path, const Schema* schema, ERRMSG);
char* mlc_hash(const absptr_t data, const Schema* schema, ERRMSG);
char* mlc_show(const absptr_t data, const Schema* schema, ERRMSG);
void* mlc_read(const char* json_str, const Schema* schema, ERRMSG);
relptr_t write_voidstar_binary(int fd, const void* data, const Schema* schema, ERRMSG);

// ── Stream-handle intrinsics ─────────────────────────────────────────────
// Open a STREAM_PACKET file as the given handle kind (MLC_KIND_*) and
// return a 64-bit handle ID. Returns -1 on failure, with errmsg set.
int64_t mlc_open(const char* path, uint8_t kind, ERRMSG);
// Close any open handle, bumping its registry slot's generation.
// Returns 0 on success, non-zero on failure.
int32_t mlc_close(int64_t handle, ERRMSG);
// Read a file's schema string without opening it as a typed handle.
// Caller frees the returned C string.
char* mlc_fschema(const char* path, ERRMSG);
// One runtime arg to mlc_ifile_walk. `has` encodes presence (0 = absent /
// use default, 1 = use `value`). `value` carries the i64 payload (an
// index for `.[]` steps, one of start/stop/step for `.[:]` steps).
// Field steps consume no args; bracket-index consumes 1; bracket-slice
// consumes 3. Args appear in DFS order matching the path.
typedef struct {
    uint8_t has;
    uint8_t _pad[7];
    int64_t value;
} mlc_ifile_walk_arg;
// Unified IFile pattern walker. The `path` string encodes a walk-step
// chain:
//   .<int>   - tuple/record-index field step
//   .<name>  - record-field-key step
//   .[]      - bracket-index leaf  (consumes 1 arg)
//   .[:]     - bracket-slice leaf  (consumes 3 args)
// Returns a fresh SHM voidstar block holding the materialized value.
// Caller frees via shfree. Errors propagate through errmsg.
void* mlc_ifile_walk(int64_t handle,
                     const char* path,
                     const mlc_ifile_walk_arg* args,
                     uint64_t n_args,
                     ERRMSG);
// Total element count of an IFile (`length f`). Returns -1 on error.
// Errors cleanly when the IFile's value type is not a list.
int64_t mlc_ifile_length(int64_t handle, ERRMSG);

// ── Cross-pool handle wire codec ─────────────────────────────────────────
//
// Stream handles are slot ids in a process-local registry. To cross a
// pool boundary the sender's bridge writes the handle's path (and kind)
// on the wire; the receiver's bridge re-opens that path in its own
// registry and binds a fresh local handle. Each pool owns its own fd +
// mmap and is responsible for closing its own slot (typically via the
// arena that recorded it).
//
// Owned strings: the path returned by mlc_handle_pack_path is a malloc'd
// NUL-terminated C string. Caller frees with libc::free.
char* mlc_handle_pack_path(int64_t handle, uint8_t* out_kind, ERRMSG);
int64_t mlc_handle_unpack_path(const char* path, uint8_t kind, ERRMSG);

// Length of the path string a handle would pack to, without allocating
// the string itself. Used by per-language get_shm_size's IFile-array
// sizing pass to size SHM exactly rather than reserve PATH_MAX per
// element. Returns the byte length, or -1 on error.
int64_t mlc_handle_path_len(int64_t handle, ERRMSG);

// Voidstar wire-layout helpers. `dest` is the Array slot in the
// voidstar buffer; `cursor` is the bridge's outer to_voidstar cursor
// (advanced past the path bytes on success). Returns 0 on success.
// On the read side, `base_ptr` is the relptr base: NULL means the
// Array's `data` field is an SHM-relative relptr (the common
// in-process case); non-NULL means it's a payload-relative offset
// (used when reading from mmap'd file regions).
int32_t mlc_write_handle_voidstar(int64_t handle, void* dest,
                                  void** cursor, ERRMSG);
int64_t mlc_read_handle_voidstar(const void* arr, const void* base_ptr,
                                 uint8_t kind, ERRMSG);

// Batched IFile-array helpers. Each acquires the stream registry mutex
// once and walks N handles, instead of N short critical sections through
// the singular helpers above. The bridges' std::vector / list / ndarray
// overloads route IFile arrays through these.
//
// `mlc_handles_path_lens` returns the sum of all handles' path byte
// lengths, or -1 on error. When `out_lens` is non-NULL, the per-handle
// lengths are also written there; sum-only callers may pass NULL.
//
// `mlc_write_handles_voidstar` writes N consecutive Array slots starting
// at `dest`, with `elem_stride` bytes between slots (= sizeof(Array)
// for a packed array). `cursor` is advanced past the concatenated path
// bytes. Returns 0 on success, nonzero on error.
int64_t mlc_handles_path_lens(const int64_t* handles, size_t n,
                              int64_t* out_lens, ERRMSG);
int32_t mlc_write_handles_voidstar(const int64_t* handles, size_t n,
                                   void* dest, size_t elem_stride,
                                   void** cursor, ERRMSG);

// IStream: forward-only file readers.
//
// `mlc_next(handle)` materialises the IStream's current sub-packet into
// a fresh SHM block typed `[a]` (an Array struct, with element + sub-
// allocation bytes following), advances the IStream cursor, and returns
// the AbsPtr to the Array. At EOF the returned Array has `size == 0`
// and `data == RELNULL`; the per-language wrapper surfaces this as the
// language's empty list. On error returns NULL with errmsg set.
//
// `mlc_stream(ifile_handle)` opens a fresh IStream slot bound to the
// same path as the source IFile (independent fd + mmap + cursor). The
// new handle is auto-registered with the current `eval_arena`.
void* mlc_next(int64_t handle, ERRMSG);
int64_t mlc_stream(int64_t ifile_handle, ERRMSG);

// OStream: forward-only file writers.
//
// `mlc_open_ostream(schema_str, path)` is the typed-open path for
// `@open path :: <IO> (OStream T)`. The codegen has the element schema
// string for T in hand at compile time. The runtime creates the file
// (O_CREAT|O_EXCL), flock'-acquires it, writes the stream header with
// the schema metadata block, and returns a fresh OSTREAM slot handle.
//
// `mlc_write(level, handle, payload_voidstar)` writes one sub-packet
// of element-list type [T]. `payload_voidstar` points to a SHM Array
// struct (the user's `[T]` value materialised by the bridge's
// `to_voidstar<vector<T>>`). `level` is the zstd compression level (0
// = uncompressed); the first @write fixes the level for the file's
// lifetime, subsequent writes must match.
//
// `mlc_append(schema_str, path)` opens an existing OStream file for
// append: forward-scans to find the last complete sub-packet, truncates
// any partial trailing bytes, re-opens with append semantics, and
// returns a fresh OSTREAM handle whose cursor sits at the resume offset.
// The schema must match the file's stored schema (mismatches error
// before any bytes are written).
//
// `mlc_concat(paths, n_paths, dest)` concatenates a sequence of stream
// files via sendfile, exploiting the stream-packet concat invariant.
// The dest file is created with a merged final footer.
int64_t mlc_open_ostream(const char* schema_str, const char* path, ERRMSG);
// @stdin / @stdout / @stderr intrinsics -- open implied. The nexus is the
// sole owner of fd 0/1/2; these register slots that route mlc_next /
// mlc_write through the pool-nexus RPC socket. At most one open per
// stdio kind per nexus (enforced by CAS in the shared registry).
int64_t mlc_open_stdin (const char* schema_str, ERRMSG);
int64_t mlc_open_stdout(const char* schema_str, ERRMSG);
int64_t mlc_open_stderr(const char* schema_str, ERRMSG);
int32_t mlc_write(uint8_t level, int64_t handle,
                  const void* payload_voidstar, ERRMSG);
int64_t mlc_append(const char* schema_str, const char* path, ERRMSG);
int32_t mlc_concat(const char* const* paths, size_t n_paths,
                   const char* dest, ERRMSG);
// `mlc_flush(handle)` forces any elements buffered for an OStream to
// be written as a sub-packet now (instead of waiting for the buffer
// to fill or for @close). No-op when the buffer is empty.
int32_t mlc_flush(int64_t handle, ERRMSG);

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
