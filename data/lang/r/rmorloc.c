#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Arith.h>

#include <stdint.h>
#include <stdbool.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/prctl.h>
#endif

#include "morloc.h"

// {{{ macros

#define MAYFAIL char* child_errmsg_ = NULL;

#define R_TRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        error("Error in R pool (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

#define R_TRY_WITH(clean, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &child_errmsg_); \
    if(child_errmsg_ != NULL){ \
        clean; \
        error("Error in R pool (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, child_errmsg_); \
    }

#define MORLOC_ERROR(msg, ...) error("Error in R pool (%s:%d in %s):" msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__);

/// }}}

// {{{ shm_tracker
//
// Deferred-cleanup list of SHM blocks that morloc_put_value handed off to a
// PACKET_SOURCE_RPTR result packet. The voidstar's relptr is shipped to the
// caller; the pool retains the only reference until the next request. We
// flush at the start of every request (in run_job_c) so blocks from the
// prior request are released before any new allocations land. Mirrors the
// pattern in pymorloc.c and pool.cpp.

#define SHM_TRACKER_INIT_CAP 16
typedef struct {
    absptr_t ptr;
    Schema* schema;  // owned by the tracker; freed in shm_tracker_flush
} shm_entry_t;
static shm_entry_t* shm_tracker = NULL;
static size_t shm_tracker_count = 0;
static size_t shm_tracker_cap = 0;

static void shm_tracker_push(absptr_t ptr, Schema* schema) {
    if (shm_tracker_count >= shm_tracker_cap) {
        size_t new_cap = shm_tracker_cap ? shm_tracker_cap * 2 : SHM_TRACKER_INIT_CAP;
        shm_entry_t* new_buf = (shm_entry_t*)realloc(shm_tracker, new_cap * sizeof(shm_entry_t));
        if (!new_buf) return;  // best-effort: drop the tracking entry on OOM
        shm_tracker = new_buf;
        shm_tracker_cap = new_cap;
    }
    shm_tracker[shm_tracker_count].ptr = ptr;
    shm_tracker[shm_tracker_count].schema = schema;
    shm_tracker_count++;
}

static void shm_tracker_flush(void) {
    for (size_t i = 0; i < shm_tracker_count; i++) {
        char* err = NULL;
        // shfree decrements the refcount and zeros the block on final
        // ref-drop, so a separate metadata-zeroing pass is unnecessary.
        shfree(shm_tracker[i].ptr, &err);
        if (err) { free(err); }
        if (shm_tracker[i].schema) {
            free_schema(shm_tracker[i].schema);
        }
    }
    shm_tracker_count = 0;
}

// Drop one tracker entry matching ptr (swap-with-last), shfree the
// block, and free its schema. Used by morloc_release_packet_shm to
// free a morloc_put_value-tracked packet's SHM as soon as its
// codegen-determined scope ends, rather than waiting for the next
// dispatch flush.
static bool shm_tracker_release_one(absptr_t ptr) {
    for (size_t i = 0; i < shm_tracker_count; i++) {
        if (shm_tracker[i].ptr == ptr) {
            Schema* schema = shm_tracker[i].schema;
            shm_tracker[i] = shm_tracker[shm_tracker_count - 1];
            shm_tracker_count--;
            char* err = NULL;
            shfree(ptr, &err);
            if (err) { free(err); }
            if (schema) { free_schema(schema); }
            return true;
        }
    }
    return false;
}

/// }}}

// ── Recursive-record env (named-schema stack) ─────────────────────────────
//
// Mirrors the pymorloc.c stack: thread-local push/pop discipline so the
// schema walkers can resolve MORLOC_RECUR back-references to their
// declarations on the way in or out.
typedef struct {
    const char* name;
    const Schema* schema;
} recur_env_entry_t;

#define RECUR_ENV_MAX 64
static __thread recur_env_entry_t recur_env_stack[RECUR_ENV_MAX];
static __thread int recur_env_depth = 0;

static int recur_env_push(const Schema* schema) {
    if (schema == NULL || schema->name == NULL || schema->type == MORLOC_RECUR) return 0;
    if (recur_env_depth >= RECUR_ENV_MAX) return 0;
    recur_env_stack[recur_env_depth].name = schema->name;
    recur_env_stack[recur_env_depth].schema = schema;
    recur_env_depth++;
    return 1;
}

static void recur_env_pop(int pushed) {
    if (pushed && recur_env_depth > 0) recur_env_depth--;
}

static const Schema* recur_env_lookup(const char* name) {
    if (name == NULL) return NULL;
    for (int i = recur_env_depth - 1; i >= 0; i--) {
        const recur_env_entry_t* e = &recur_env_stack[i];
        if (e->name != NULL && strcmp(e->name, name) == 0) {
            return e->schema;
        }
    }
    return NULL;
}

// {{{ bit64 (integer64) helpers
//
// Int64 / UInt64 / IFile handles are mapped to the `bit64::integer64`
// R class (stdlib/root-r/main.loc). integer64 is implemented as a
// REALSXP whose 8 bytes per cell are reinterpreted as int64_t; the S3
// class attribute "integer64" tags the storage. The R-level package
// `bit64` must be loaded for downstream R code to do arithmetic on
// these vectors; the C runtime itself only needs the in-band byte
// layout, which we marshal here.

static int is_integer64(SEXP obj) {
    if (TYPEOF(obj) != REALSXP) return 0;
    SEXP cls = getAttrib(obj, R_ClassSymbol);
    if (cls == R_NilValue || TYPEOF(cls) != STRSXP) return 0;
    for (R_xlen_t i = 0; i < LENGTH(cls); i++) {
        if (strcmp(CHAR(STRING_ELT(cls, i)), "integer64") == 0) {
            return 1;
        }
    }
    return 0;
}

// Allocate a fresh REALSXP and tag it with class "integer64". Caller
// is responsible for PROTECTing the result if it escapes into a
// longer-lived scope.
static SEXP alloc_integer64(R_xlen_t n) {
    SEXP v = PROTECT(allocVector(REALSXP, n));
    SEXP cls = PROTECT(mkString("integer64"));
    setAttrib(v, R_ClassSymbol, cls);
    UNPROTECT(2);
    return v;
}

// Wrap a single int64_t as a length-1 integer64 SEXP.
static SEXP make_integer64_scalar(int64_t v) {
    SEXP s = PROTECT(alloc_integer64(1));
    memcpy(&REAL(s)[0], &v, sizeof(int64_t));
    UNPROTECT(1);
    return s;
}

// Read an int64_t from an R SEXP. Accepts:
//   * INTSXP            -- widen from int32
//   * REALSXP integer64 -- bit-reinterpret 8 bytes as int64
//   * REALSXP plain     -- truncate the double (range-checked by callers)
// Returns 0 on type error so callers handle it via their own guards.
static int64_t i64_from_sexp(SEXP obj) {
    if (TYPEOF(obj) == INTSXP) {
        return (int64_t)INTEGER(obj)[0];
    }
    if (TYPEOF(obj) == REALSXP) {
        if (is_integer64(obj)) {
            int64_t v;
            memcpy(&v, &REAL(obj)[0], sizeof(int64_t));
            return v;
        }
        return (int64_t)REAL(obj)[0];
    }
    return 0;
}

// }}} bit64 helpers

// {{{ to_voidstar

static size_t get_shm_size_inner(const Schema* schema, SEXP obj);

// Public wrapper: maintain the recur env stack across the recursive walk
// so MORLOC_RECUR arms can resolve their back-reference targets.
static size_t get_shm_size(const Schema* schema, SEXP obj) {
    int pushed = recur_env_push(schema);
    size_t r = get_shm_size_inner(schema, obj);
    recur_env_pop(pushed);
    return r;
}

static size_t get_shm_size_inner(const Schema* schema, SEXP obj) {
    size_t size = 0;
    switch (schema->type) {
        case MORLOC_NIL:
        case MORLOC_BOOL:
        case MORLOC_SINT8:
        case MORLOC_SINT16:
        case MORLOC_SINT32:
        case MORLOC_SINT64:
        case MORLOC_UINT8:
        case MORLOC_UINT16:
        case MORLOC_UINT32:
        case MORLOC_UINT64:
        case MORLOC_FLOAT32:
        case MORLOC_FLOAT64:
            return schema->width;
        case MORLOC_INT:
            // Inline BigInt: R values always fit inline (16 bytes)
            return 16;
        case MORLOC_IFILE:
        case MORLOC_OSTREAM:
        case MORLOC_ISTREAM: {
            // Stream-handle field: look up the exact suballoc cost via
            // the registry (returns 8 + path_len for TAG_PATH, 0 for
            // empty).
            int64_t handle = i64_from_sexp(obj);
            char* err = NULL;
            int64_t n = mlc_handle_path_len(handle, &err);
            if (n < 0) {
                char msg[512];
                snprintf(msg, sizeof(msg), "mlc_handle_path_len: %s",
                         err ? err : "(null)");
                free(err);
                MORLOC_ERROR("%s", msg);
            }
            return sizeof(Array) + (size_t)n;
        }
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                size_t length = (size_t)LENGTH(obj);
                size = sizeof(Array);
                // worst-case cursor alignment padding for element data.
                // String stays at natural element alignment (1 byte for chars);
                // Array bumps to 64 for primitive numeric elements (SIMD/BLAS).
                size_t buf_align = (schema->type == MORLOC_STRING)
                    ? schema_alignment(schema->parameters[0])
                    : array_data_alignment(schema->parameters[0]);
                size += buf_align - 1;
                // Array of IFile handles: each element is wire-encoded as
                // Array<u8>(path). The REALSXP fast-path below would treat
                // bit64-backed IFile handles as fixed-width 8-byte ints
                // and silently under-count by path_len per element. Use
                // the batched registry call so we acquire the stream-
                // registry mutex once instead of N times.
                if (schema->type == MORLOC_ARRAY
                    && (schema->parameters[0]->type == MORLOC_IFILE
                     || schema->parameters[0]->type == MORLOC_OSTREAM
                     || schema->parameters[0]->type == MORLOC_ISTREAM)) {
                    int is_b64 = is_integer64(obj);
                    int is_list = (TYPEOF(obj) == VECSXP);
                    if (!is_b64 && !is_list) {
                        MORLOC_ERROR(
                            "Expected list or bit64::integer64 vector for [IFile a],"
                            " got %s", type2char(TYPEOF(obj)));
                    }
                    // bit64 stores int64 directly in REALSXP slots; pass
                    // the buffer through. Lists must be extracted first.
                    const int64_t* handles;
                    if (is_b64) {
                        handles = (const int64_t*)REAL(obj);
                    } else {
                        int64_t* scratch = (int64_t*)R_alloc(length, sizeof(int64_t));
                        for (size_t i = 0; i < length; i++) {
                            scratch[i] = i64_from_sexp(VECTOR_ELT(obj, i));
                        }
                        handles = scratch;
                    }
                    char* child_errmsg_ = NULL;
                    int64_t paths_total = R_TRY(mlc_handles_path_lens,
                        handles, length, NULL);
                    size += length * sizeof(Array) + (size_t)paths_total;
                    return size;
                }
                const char* str;

                // R's NULL stands in for a zero-length vector (e.g. an
                // R function returning numeric(0) may surface as NULL
                // through some code paths, and a literal `[] :: Vector
                // 0 a` deserialised on the wire-empty branch is
                // represented as NULL). Treat it as an empty array
                // with no element data; the inner switch's TYPE-based
                // arms would otherwise fall to default and error.
                if (TYPEOF(obj) == NILSXP) {
                    return size;
                }

                switch (TYPEOF(obj)) {
                    case CHARSXP:
                        str = CHAR(obj);
                        size += (size_t)strlen(str);  // Do not include null terminator
                        break;
                    case STRSXP:
                        if (LENGTH(obj) == 1) {
                            str = CHAR(STRING_ELT(obj, 0));
                            size += (size_t)strlen(str);  // Do not include null terminator
                        } else {
                            if(schema->parameters[0]->type == MORLOC_STRING){
                                for(size_t i = 0; i < length; i++){
                                    size += get_shm_size(schema->parameters[0], STRING_ELT(obj, i));
                                }
                            } else {
                                MORLOC_ERROR("Expected character vector of length 1, but got length %zu", length);
                            }
                        }
                        break;
                    case VECSXP:  // This handles lists
                        for (int i = 0; i < length; i++) {
                            size += get_shm_size(schema->parameters[0], VECTOR_ELT(obj, i));
                        }
                        break;
                    case LGLSXP:
                    case INTSXP:
                    case REALSXP:
                    case RAWSXP:
                        {
                            // Inline BigInt and fixed-width types: width covers the full element
                            size += length * schema->parameters[0]->width;
                        }
                        break;
                    default:
                        MORLOC_ERROR("Unsupported type in get_shm_size array: %s", type2char(TYPEOF(obj)));
                }
                return size;
            }

        case MORLOC_TUPLE:
            if (!isVectorList(obj)) {
                MORLOC_ERROR("Expected list for MORLOC_TUPLE, but got %s", type2char(TYPEOF(obj)));
            }

            {
                size_t array_size = (size_t)xlength(obj);
                if (array_size != schema->size) {
                    MORLOC_ERROR("Expected tuple of length %zu, but found list of length %zu", schema->size, size);
                }
                size = schema->width;
                for (R_xlen_t i = 0; i < (R_xlen_t)array_size; ++i) {
                    SEXP item = VECTOR_ELT(obj, i);
                    size_t elem = get_shm_size(schema->parameters[i], item);
                    if (elem > schema->parameters[i]->width) {
                        size += elem - schema->parameters[i]->width;
                    }
                }
                return size;
            }

        case MORLOC_MAP:
            {
                if (isNewList(obj)) {
                    // Handle named list
                    size = schema->width;
                    SEXP names = getAttrib(obj, R_NamesSymbol);
                    if (names == R_NilValue) {
                        error("List must have names for MORLOC_MAP");
                    }
                    for (size_t i = 0; i < schema->size; ++i) {
                        SEXP key = PROTECT(mkChar(schema->keys[i]));
                        int index = -1;
                        for (int j = 0; j < length(obj); j++) {
                            if (strcmp(CHAR(STRING_ELT(names, j)), CHAR(key)) == 0) {
                                index = j;
                                break;
                            }
                        }
                        if (index != -1) {
                            SEXP value = VECTOR_ELT(obj, index);
                            size_t elem = get_shm_size(schema->parameters[i], value);
                            if (elem > schema->parameters[i]->width) {
                                size += elem - schema->parameters[i]->width;
                            }
                        }
                        UNPROTECT(1);
                    }
                    return size;
                } else {
                    error("Expected a named list for MORLOC_MAP");
                }
            }

        case MORLOC_OPTIONAL:
            // Slot is sizeof(relptr) (= schema->width). Absent → just the slot.
            // Present → slot + worst-case alignment padding for the inner T +
            // T's total size (its own width plus any variable extras).
            if (obj == R_NilValue) {
                return schema->width;
            }
            {
                size_t inner_size = get_shm_size(schema->parameters[0], obj);
                size_t inner_align = schema_alignment(schema->parameters[0]);
                if (inner_align == 0) inner_align = 1;
                return schema->width + (inner_align - 1) + inner_size;
            }

        case MORLOC_RECUR: {
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                MORLOC_ERROR("Recur back-reference to undeclared schema name '%s'",
                             schema->name ? schema->name : "?");
            }
            return get_shm_size_inner(target, obj);
        }

        default:
            MORLOC_ERROR("Unhandled schema type %d in get_size_inner", (int)schema->type);
            break;
    }

    return size;
}


// IEEE 754 double can exactly represent every integer in [-2^53, 2^53].
// Beyond that, fixed-width 64-bit integers cannot survive a round-trip
// through R's "numeric" storage: nearby int64 / uint64 values collapse to
// the same double, and casting back is silent corruption. Use this bound
// as the threshold for the SINT64 / UINT64 / MORLOC_INT paths instead of
// (double)INT64_MAX, which itself rounds to 2^63 and defeats the check.
#define R_DOUBLE_INT_MAX 9007199254740992LL  // 2^53

// NA/NaN rejection: NaN comparisons are always false, so the range checks
// below silently admit any NaN (including R's NA_integer_/NA_real_, which
// asReal collapses to NA_REAL). Casting NaN to an integer is undefined
// behaviour and typically wraps to INT*_MIN. Reject explicitly.
#define HANDLE_SINT_TYPE(CTYPE, MIN, MAX) \
    do { \
        if (!(isInteger(obj) || isReal(obj))) { \
            MORLOC_ERROR("Expected integer for %s, but got %s", #CTYPE, type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (ISNAN(value)) { \
            MORLOC_ERROR("Cannot pack NA or NaN into %s", #CTYPE); \
        } \
        if (value < MIN || value > MAX) { \
            MORLOC_ERROR("Integer overflow for %s", #CTYPE); \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)

#define HANDLE_UINT_TYPE(CTYPE, MAX) \
    do { \
        if (!(isInteger(obj) || isReal(obj))) { \
            MORLOC_ERROR("Expected integer for %s, but got %s", #CTYPE, type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (ISNAN(value)) { \
            MORLOC_ERROR("Cannot pack NA or NaN into %s", #CTYPE); \
        } \
        if (value < 0 || value > MAX) { \
            MORLOC_ERROR("Integer overflow for %s", #CTYPE); \
        } \
        *(CTYPE*)dest = (CTYPE)value; \
    } while(0)

// 64-bit fixed-width: distinct from HANDLE_*_TYPE because the full 64-bit
// range is only representable when the SEXP arrives as a bit64::integer64
// (REALSXP whose 8 bytes are an int64). For integer64 values the bits are
// memcpy'd directly -- no double round-trip, so the full [-2^63, 2^63-1]
// range round-trips losslessly. Plain REALSXP / INTSXP still work for
// convenience (e.g. integer literals from R user code), with the existing
// 2^53 precision ceiling.
#define HANDLE_SINT64() \
    do { \
        if (is_integer64(obj)) { \
            int64_t v; \
            memcpy(&v, &REAL(obj)[0], sizeof(int64_t)); \
            *(int64_t*)dest = v; \
            break; \
        } \
        if (!(isInteger(obj) || isReal(obj))) { \
            MORLOC_ERROR("Expected integer for int64_t, but got %s", type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (ISNAN(value)) { \
            MORLOC_ERROR("Cannot pack NA or NaN into int64_t"); \
        } \
        if (value != trunc(value)) { \
            MORLOC_ERROR("Expected integer for int64_t, got non-integer numeric %.17g", value); \
        } \
        if (value < -(double)R_DOUBLE_INT_MAX || value > (double)R_DOUBLE_INT_MAX) { \
            MORLOC_ERROR( \
                "Integer %.0f does not fit in R's numeric type" \
                " (max 2^53 = 9007199254740992 for integer precision)." \
                " Wrap with bit64::as.integer64() for the full Int64 range.", \
                value); \
        } \
        *(int64_t*)dest = (int64_t)value; \
    } while(0)

#define HANDLE_UINT64() \
    do { \
        if (is_integer64(obj)) { \
            int64_t v; \
            memcpy(&v, &REAL(obj)[0], sizeof(int64_t)); \
            *(uint64_t*)dest = (uint64_t)v; \
            break; \
        } \
        if (!(isInteger(obj) || isReal(obj))) { \
            MORLOC_ERROR("Expected integer for uint64_t, but got %s", type2char(TYPEOF(obj))); \
        } \
        double value = asReal(obj); \
        if (ISNAN(value)) { \
            MORLOC_ERROR("Cannot pack NA or NaN into uint64_t"); \
        } \
        if (value != trunc(value)) { \
            MORLOC_ERROR("Expected integer for uint64_t, got non-integer numeric %.17g", value); \
        } \
        if (value < 0 || value > (double)R_DOUBLE_INT_MAX) { \
            MORLOC_ERROR( \
                "Unsigned integer %.0f does not fit in R's numeric type" \
                " (max 2^53 = 9007199254740992 for integer precision)." \
                " Wrap with bit64::as.integer64() for the full UInt64 range.", \
                value); \
        } \
        *(uint64_t*)dest = (uint64_t)value; \
    } while(0)

static void* to_voidstar_inner_impl(void* dest, void** cursor, SEXP obj, const Schema* schema);

// Public entry point: push the schema's declaration name (if any) onto
// the recur env, dispatch to the inner walker, then pop. Recur arms
// inside _inner resolve targets via recur_env_lookup.
static void* to_voidstar_inner(void* dest, void** cursor, SEXP obj, const Schema* schema){
    int pushed = recur_env_push(schema);
    void* r = to_voidstar_inner_impl(dest, cursor, obj, schema);
    recur_env_pop(pushed);
    return r;
}

static void* to_voidstar_inner_impl(void* dest, void** cursor, SEXP obj, const Schema* schema){
    MAYFAIL

    switch (schema->type) {
        case MORLOC_NIL:
            if (obj != R_NilValue) {
                MORLOC_ERROR("Expected NULL for MORLOC_NIL, but got %s", type2char(TYPEOF(obj)));
            }
            *((int8_t*)dest) = (int8_t)0;
            break;
        case MORLOC_BOOL:
            if (!isLogical(obj)) {
                MORLOC_ERROR("Expected logical for MORLOC_BOOL, but got %s", type2char(TYPEOF(obj)));
            }
            *((uint8_t*)dest) = (uint8_t)((LOGICAL(obj)[0] == TRUE) ? 1 : 0);
            break;
        case MORLOC_SINT8:
            HANDLE_SINT_TYPE(int8_t, INT8_MIN, INT8_MAX);
            break;
        case MORLOC_SINT16:
            HANDLE_SINT_TYPE(int16_t, INT16_MIN, INT16_MAX);
            break;
        case MORLOC_SINT32:
            HANDLE_SINT_TYPE(int32_t, INT32_MIN, INT32_MAX);
            break;
        case MORLOC_SINT64:
            HANDLE_SINT64();
            break;
        case MORLOC_UINT8:
            HANDLE_UINT_TYPE(uint8_t, UINT8_MAX);
            break;
        case MORLOC_UINT16:
            HANDLE_UINT_TYPE(uint16_t, UINT16_MAX);
            break;
        case MORLOC_UINT32:
            HANDLE_UINT_TYPE(uint32_t, UINT32_MAX);
            break;
        case MORLOC_UINT64:
            HANDLE_UINT64();
            break;
        case MORLOC_INT: {
            // Inline BigInt: [size=1, value] — no allocation needed
            if (!(isInteger(obj) || isReal(obj))) {
                MORLOC_ERROR("Expected integer or numeric for MORLOC_INT, but got %s", type2char(TYPEOF(obj)));
            }
            double value = asReal(obj);
            if (ISNAN(value)) {
                MORLOC_ERROR("Cannot pack NA or NaN into MORLOC_INT");
            }
            if (value != trunc(value)) {
                MORLOC_ERROR("Expected integer value for MORLOC_INT, got non-integer double");
            }
            if (value < -(double)R_DOUBLE_INT_MAX || value > (double)R_DOUBLE_INT_MAX) {
                // (double)INT64_MAX rounds to 2^63 and defeats the bound check;
                // anything outside [-2^53, 2^53] cannot have come from R as a
                // precise integer, so reject rather than silently corrupt.
                MORLOC_ERROR(
                    "Integer %.0f does not fit in R's numeric type"
                    " (max 2^53 = 9007199254740992 for integer precision).",
                    value);
            }
            int64_t* fields = (int64_t*)dest;
            fields[0] = 1;
            fields[1] = (int64_t)value;
            break;
        }
        case MORLOC_FLOAT32:
            if (!(isReal(obj) || isInteger(obj))) {
                MORLOC_ERROR("Expected numeric for MORLOC_FLOAT32, but got %s", type2char(TYPEOF(obj)));
            }
            *((float*)dest) = (float)asReal(obj);
            break;

        case MORLOC_FLOAT64:
            if (!(isReal(obj) || isInteger(obj))) {
                MORLOC_ERROR("Expected numeric for MORLOC_FLOAT64, but got %s", type2char(TYPEOF(obj)));
            }
            *((double*)dest) = asReal(obj);
            break;
        case MORLOC_IFILE:
        case MORLOC_OSTREAM:
        case MORLOC_ISTREAM: {
            int64_t handle = i64_from_sexp(obj);
            R_TRY(mlc_write_handle_voidstar, handle, dest, cursor);
            break;
        }
        case MORLOC_STRING:
            {
                const char* str = NULL;
                size_t length = 0;
                switch(TYPEOF(obj)){
                    case CHARSXP:
                        str = CHAR(obj);
                        length = (size_t)strlen(str);
                        break;
                    case STRSXP:
                        if (LENGTH(obj) == 1) {
                            str = CHAR(STRING_ELT(obj, 0));
                            length = (size_t)strlen(str);
                        } else {
                            MORLOC_ERROR("Expected character of length 1");
                        }
                        break;
                    case RAWSXP:
                        str = RAW(obj);
                        length = LENGTH(obj);
                        break;
                    default:
                      MORLOC_ERROR("Expected a character type");
                      break;
                }
                Array* array = (Array*)dest;
                array->size = length;  // Do not include null terminator
                if(length > 0){
                    // String character data: natural alignment (1 byte for chars)
                    *cursor = (void*)ALIGN_UP((uintptr_t)*cursor, schema_alignment(schema->parameters[0]));
                    array->data = R_TRY(abs2rel, *cursor);
                    absptr_t tmp_ptr = R_TRY(rel2abs, array->data);
                    memcpy(tmp_ptr, str, array->size);
                } else {
                    array->data = RELNULL;
                }

                // move cursor to the location after the copied data
                *cursor = (void*)(*(char**)cursor + array->size);
            }
            break;
        case MORLOC_ARRAY:
            Array* array = (Array*)dest;
            array->size = (size_t)length(obj);
            if(array->size == 0){
                array->data = RELNULL;
                break;
            }

            // align cursor for element data placement
            // (bumps to 64 for primitive numerics for SIMD/BLAS)
            *cursor = (void*)ALIGN_UP((uintptr_t)*cursor, array_data_alignment(schema->parameters[0]));
            array->data = R_TRY(abs2rel, *cursor);
            Schema* element_schema = schema->parameters[0];
            char* start;

            // Batched stream-handle array write: one registry lock for
            // all N handles. Accepts both VECSXP (list of bit64 scalars)
            // and REALSXP (bit64 vector, which stores int64 in REAL
            // slots).
            if ((element_schema->type == MORLOC_IFILE
                 || element_schema->type == MORLOC_OSTREAM
                 || element_schema->type == MORLOC_ISTREAM)
                && (TYPEOF(obj) == VECSXP
                    || (TYPEOF(obj) == REALSXP && is_integer64(obj)))) {
                const int64_t* handles;
                if (TYPEOF(obj) == REALSXP) {
                    handles = (const int64_t*)REAL(obj);
                } else {
                    int64_t* scratch = (int64_t*)R_alloc(array->size, sizeof(int64_t));
                    for (size_t i = 0; i < array->size; i++) {
                        scratch[i] = i64_from_sexp(VECTOR_ELT(obj, i));
                    }
                    handles = scratch;
                }
                *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                start = R_TRY(rel2abs, array->data);
                R_TRY(mlc_write_handles_voidstar,
                    handles, array->size, start, element_schema->width, cursor);
                break;
            }

            switch (TYPEOF(obj)) {
                case STRSXP:
                    {
                        if(element_schema->type == MORLOC_STRING){
                            // set the cursor the the location after the array headers
                            *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                            start = R_TRY(rel2abs, array->data);
                            for(size_t i = 0; i < array->size; i++){
                                SEXP elem = STRING_ELT(obj, i);
                                to_voidstar_inner(start + i * element_schema->width, cursor, elem, element_schema);
                            }
                        } else {
                            MORLOC_ERROR("Expected character vector of length 1, but got length %ld", array->size);
                        }
                    }
                    break;
                case RAWSXP:  // Raw vectors
                    if (element_schema->type != MORLOC_UINT8) {
                        MORLOC_ERROR("Expected MORLOC_UINT8 for raw vector");
                    }
                    absptr_t tmp_ptr = R_TRY(rel2abs, array->data);
                    memcpy(tmp_ptr, RAW(obj), array->size * sizeof(uint8_t));
                    *cursor = (void*)(*(char**)cursor + array->size * sizeof(uint8_t));
                    break;
                case VECSXP:  // This handles lists
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = VECTOR_ELT(obj, i);
                        to_voidstar_inner(start + i * element_schema->width, cursor, elem, element_schema);
                    }
                    break;
                case LGLSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarLogical(LOGICAL(obj)[i]));
                        to_voidstar_inner(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case INTSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarInteger(INTEGER(obj)[i]));
                        to_voidstar_inner(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                case REALSXP:
                    *cursor = (void*)(*(char**)cursor + array->size * element_schema->width);
                    start = R_TRY(rel2abs, array->data);
                    // bit64::integer64 storage: REALSXP's 8 bytes per
                    // cell ARE the int64. For SINT64 / UINT64 element
                    // schemas we can memcpy the whole vector directly
                    // and bypass the per-element ScalarReal round-trip
                    // (which would strip the integer64 class).
                    if (is_integer64(obj) && (element_schema->type == MORLOC_SINT64
                                              || element_schema->type == MORLOC_UINT64)) {
                        memcpy(start, REAL(obj), array->size * sizeof(int64_t));
                        break;
                    }
                    for (int i = 0; i < array->size; i++) {
                        SEXP elem = PROTECT(ScalarReal(REAL(obj)[i]));
                        to_voidstar_inner(start + i * element_schema->width, cursor, elem, element_schema);
                        UNPROTECT(1);
                    }
                    break;
                default:
                    MORLOC_ERROR("Unsupported type in to_voidstar array: %s", type2char(TYPEOF(obj)));
            }
            break;



        case MORLOC_TUPLE:
            if (!isVectorList(obj)) {
                MORLOC_ERROR("Expected list for MORLOC_TUPLE, but got %s", type2char(TYPEOF(obj)));
            }

            {
                R_xlen_t size = xlength(obj);
                if ((size_t)size != schema->size) {
                    MORLOC_ERROR("Expected tuple of length %zu, but found list of length %zu", schema->size, size);
                }
                for (R_xlen_t i = 0; i < size; ++i) {
                    SEXP item = VECTOR_ELT(obj, i);
                    to_voidstar_inner(dest + schema->offsets[i], cursor, item, schema->parameters[i]);
                }
            }
            break;

        case MORLOC_MAP:
            {
                if (isNewList(obj)) {
                    // Handle named list
                    SEXP names = getAttrib(obj, R_NamesSymbol);
                    if (names == R_NilValue) {
                        MORLOC_ERROR("List must have names for MORLOC_MAP");
                    }
                    for (size_t i = 0; i < schema->size; ++i) {
                        SEXP key = PROTECT(mkChar(schema->keys[i]));
                        int index = -1;
                        for (int j = 0; j < length(obj); j++) {
                            if (strcmp(CHAR(STRING_ELT(names, j)), CHAR(key)) == 0) {
                                index = j;
                                break;
                            }
                        }
                        if (index != -1) {
                            SEXP value = VECTOR_ELT(obj, index);
                            to_voidstar_inner(dest + schema->offsets[i], cursor, value, schema->parameters[i]);
                        }
                        UNPROTECT(1);
                    }
                } else {
                    MORLOC_ERROR("Expected a named list for MORLOC_MAP");
                }
            }
            break;

        case MORLOC_OPTIONAL:
            // The slot is a relptr. Absent → write RELNULL. Present →
            // align the cursor for the inner T, write the inner's relptr
            // into the slot, advance the cursor past T's width, then
            // recurse to fill T's body.
            if (obj == R_NilValue) {
                *((relptr_t*)dest) = RELNULL;
            } else {
                const Schema* inner_schema = schema->parameters[0];
                size_t inner_align = schema_alignment(inner_schema);
                if (inner_align == 0) inner_align = 1;
                *cursor = (void*)(((uintptr_t)*cursor + inner_align - 1) & ~(uintptr_t)(inner_align - 1));
                {
                    char* rel_err = NULL;
                    *(relptr_t*)dest = abs2rel(*cursor, &rel_err);
                    if (rel_err) { free(rel_err); MORLOC_ERROR("abs2rel failed in MORLOC_OPTIONAL"); }
                }
                void* inner_dest = *cursor;
                *cursor = (void*)((char*)*cursor + inner_schema->width);
                to_voidstar_inner(inner_dest, cursor, obj, inner_schema);
            }
            break;

        case MORLOC_RECUR: {
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                MORLOC_ERROR("Recur back-reference to undeclared schema name '%s'",
                             schema->name ? schema->name : "?");
            }
            // Dispatch directly to _inner so the Recur node itself
            // does not push its own (lookup-key) name onto the env.
            to_voidstar_inner_impl(dest, cursor, obj, target);
            break;
        }

        default:
            MORLOC_ERROR("Unhandled schema type %d in to_voidstar_inner", (int)schema->type);
            break;
    }

    return dest;

}


// NOTE: If to_voidstar_inner calls error() (via MORLOC_ERROR or R_TRY), the shared
// memory at dest leaks. This only happens on type mismatches (a development-time
// bug) and the memory is reclaimed when the pool process exits.
static void* to_voidstar(SEXP obj, const Schema* schema) {
    MAYFAIL

    size_t total_size = get_shm_size(schema, obj);

    void* dest = R_TRY(shmalloc, total_size);

    void* cursor = (void*)((char*)dest + schema->width);

    return to_voidstar_inner(dest, &cursor, obj, schema);
}

// }}} to_voidstar

// {{{ from_voidstar

static SEXP from_voidstar_inner(const void* data, const Schema* schema, const void* base_ptr);

// Public entry point: push/pop the recur env around every call so the
// inner walker can resolve MORLOC_RECUR via recur_env_lookup.
static SEXP from_voidstar(const void* data, const Schema* schema, const void* base_ptr) {
    int pushed = recur_env_push(schema);
    SEXP r = from_voidstar_inner(data, schema, base_ptr);
    recur_env_pop(pushed);
    return r;
}

static SEXP from_voidstar_inner(const void* data, const Schema* schema, const void* base_ptr) {
    MAYFAIL

    if(data == NULL){
        MORLOC_ERROR("NULL data (%s:%d in %s)", __FILE__, __LINE__, __func__);
    }

    if(schema == NULL){
        MORLOC_ERROR("NULL schema (%s:%d in %s)", __FILE__, __LINE__, __func__);
    }

    SEXP obj = R_NilValue;
    switch (schema->type) {
        case MORLOC_NIL:
            return R_NilValue;
        case MORLOC_BOOL:
            obj = ScalarLogical((bool)*(uint8_t*)data);
            break;
        case MORLOC_SINT8:
            obj = ScalarInteger((int)(*(int8_t*)data));
            break;
        case MORLOC_SINT16:
            obj = ScalarInteger((int)(*(int16_t*)data));
            break;
        case MORLOC_SINT32:
            // R's integer reserves INT32_MIN as NA_integer_, so storing the
            // full Int32 range as INTSXP would conflate the legitimate value
            // -2^31 with missing data. Use REALSXP (double) instead — int32
            // fits in 53 bits, so values round-trip exactly.
            obj = ScalarReal((double)(*(int32_t*)data));
            break;
        case MORLOC_SINT64: {
            // Int64 maps to bit64::integer64 in R. The S3 class is a
            // tagged REALSXP whose 8 bytes are the int64; we memcpy the
            // value into the storage and set the class. No range check
            // is needed -- integer64 covers the full Int64 range
            // losslessly. (NA_INTEGER64 is INT64_MIN; legitimate
            // INT64_MIN values become NA, which is a known bit64
            // tradeoff; we let the wire value through unchanged.)
            int64_t val = *(int64_t*)data;
            obj = PROTECT(make_integer64_scalar(val));
            UNPROTECT(1);
            break;
        }
        case MORLOC_UINT8:
            obj = ScalarInteger((int)(*(uint8_t*)data));
            break;
        case MORLOC_UINT16:
            obj = ScalarInteger((int)(*(uint16_t*)data));
            break;
        case MORLOC_UINT32:
            obj = ScalarReal((double)(*(uint32_t*)data));
            break;
        case MORLOC_UINT64: {
            // UInt64 maps to bit64::integer64 in R. bit64 is signed,
            // so values with the high bit set will appear negative in R
            // user code -- this is the bit64 contract and matches how
            // we tag (uint64_t)val by reinterpret. No range check
            // needed; the full UInt64 range round-trips bit-for-bit.
            uint64_t val = *(uint64_t*)data;
            int64_t bits = (int64_t)val;
            obj = PROTECT(make_integer64_scalar(bits));
            UNPROTECT(1);
            break;
        }
        case MORLOC_FLOAT32:
            obj = ScalarReal((double)(*(float*)data));
            break;
        case MORLOC_FLOAT64:
            obj = ScalarReal(*(double*)data);
            break;
        case MORLOC_INT: {
            // Inline BigInt: [size:i64, value_or_relptr:i64]
            const int64_t* fields = (const int64_t*)data;
            int64_t bigint_size = fields[0];
            if (bigint_size <= 1) {
                int64_t val = (bigint_size == 0) ? 0 : fields[1];
                if (val >= INT32_MIN && val <= INT32_MAX) {
                    obj = ScalarInteger((int)val);
                } else if (val >= -R_DOUBLE_INT_MAX && val <= R_DOUBLE_INT_MAX) {
                    obj = ScalarReal((double)val);
                } else {
                    // Single-limb BigInt that fits int64 but exceeds 2^53:
                    // representable on the wire, not in R's numeric.
                    MORLOC_ERROR(
                        "Integer overflow: BigInt value %lld does not fit in R's numeric type"
                        " (max 2^53 = 9007199254740992 for integer precision).",
                        (long long)val);
                }
            } else {
                // Overflow: multi-limb integer
                MORLOC_ERROR(
                    "Integer overflow: %lld-limb integer (%lld bits)"
                    " does not fit in R's numeric type (max 2^53 for integer precision).",
                    (long long)bigint_size, (long long)(bigint_size * 64));
                // unreachable, but satisfy compiler
                const uint64_t* limbs = (const uint64_t*)resolve_relptr(
                    *(const relptr_t*)&fields[1], base_ptr, NULL);
                int64_t val = (int64_t)limbs[0];
                if (val >= INT32_MIN && val <= INT32_MAX) {
                    obj = ScalarInteger((int)val);
                } else {
                    obj = ScalarReal((double)val);
                }
            }
            break;
        }
        case MORLOC_IFILE:
        case MORLOC_OSTREAM:
        case MORLOC_ISTREAM: {
            uint8_t kind = (schema->type == MORLOC_IFILE)   ? MLC_KIND_IFILE
                         : (schema->type == MORLOC_OSTREAM) ? MLC_KIND_OSTREAM
                         :                                    MLC_KIND_ISTREAM;
            int64_t handle = R_TRY(mlc_read_handle_voidstar,
                                   data, base_ptr, kind);
            obj = make_integer64_scalar(handle);
            break;
        }
        case MORLOC_STRING: {
                if (schema->hint != NULL && strcmp(schema->hint, "raw") == 0){
                    Array* raw_array = (Array*)data;
                    if(raw_array->size > 0){
                        void* tmp_ptr = R_TRY(resolve_relptr, raw_array->data, base_ptr);
                        obj = PROTECT(allocVector(RAWSXP, raw_array->size));
                        memcpy(RAW(obj), tmp_ptr, raw_array->size);
                    } else {
                        obj = PROTECT(allocVector(RAWSXP, 0));
                    }
                    UNPROTECT(1);
                } else {
                    Array* str_array = (Array*)data;
                    if(str_array->size > 0){
                        void* tmp_ptr = R_TRY(resolve_relptr, str_array->data, base_ptr);
                        SEXP chr = PROTECT(mkCharLen(tmp_ptr, str_array->size));
                        obj = PROTECT(ScalarString(chr));
                    } else {
                        SEXP chr = PROTECT(mkChar(""));
                        obj = PROTECT(ScalarString(chr));
                    }
                    UNPROTECT(2);
                }
            }
            break;
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                Schema* element_schema = schema->parameters[0];
                char* start;

                switch(element_schema->type){
                    case MORLOC_BOOL:
                        obj = PROTECT(allocVector(LGLSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            LOGICAL(obj)[i] = (bool)*(uint8_t*)(start + i) ? TRUE : FALSE;
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT8:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int8_t*)(start + i * sizeof(int8_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(int16_t*)(start + i * sizeof(int16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT32:
                        // INT32_MIN clashes with NA_integer_ in R, so store
                        // the array as REALSXP (double). Doubles represent
                        // every int32 exactly.
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(int32_t*)(start + i * sizeof(int32_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_SINT64:
                        // bit64::integer64 vector. The REALSXP storage
                        // is bit-identical to a C int64_t array, so we
                        // memcpy the whole element block.
                        obj = PROTECT(alloc_integer64(array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        memcpy(REAL(obj), start, array->size * sizeof(int64_t));
                        UNPROTECT(1);
                        break;
                    // Interpret the uint8 as a raw vector
                    case MORLOC_UINT8:
                        obj = PROTECT(allocVector(RAWSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        memcpy(RAW(obj), start, array->size * sizeof(uint8_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT16:
                        obj = PROTECT(allocVector(INTSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            INTEGER(obj)[i] = (int)(*(uint16_t*)(start + i * sizeof(uint16_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(uint32_t*)(start + i * sizeof(uint32_t)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_UINT64:
                        // bit64::integer64 vector (bit64 is signed but
                        // we keep the wire bytes verbatim; the high bit
                        // surfaces as a negative integer64 in R, which
                        // matches bit64's documented behaviour).
                        obj = PROTECT(alloc_integer64(array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        memcpy(REAL(obj), start, array->size * sizeof(uint64_t));
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT32:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        for (size_t i = 0; i < array->size; i++) {
                            REAL(obj)[i] = (double)(*(float*)(start + i * sizeof(float)));
                        }
                        UNPROTECT(1);
                        break;
                    case MORLOC_FLOAT64:
                        obj = PROTECT(allocVector(REALSXP, array->size));
                        if(array->size == 0) {
                            UNPROTECT(1);
                            break;
                        }
                        start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                        memcpy(REAL(obj), start, array->size * sizeof(double));
                        UNPROTECT(1);
                        break;
                    case MORLOC_STRING:
                        {
                            obj = PROTECT(allocVector(STRSXP, array->size));
                            if(array->size == 0) {
                                UNPROTECT(1);
                                break;
                            }
                            start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                            size_t width = schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                Array* str_array = (Array*)(start + i * width);
                                SEXP item;
                                if(str_array->size == 0){
                                    item = PROTECT(mkCharLen("", 0));
                                } else {
                                    void* str_ptr = R_TRY_WITH(UNPROTECT(1), resolve_relptr, str_array->data, base_ptr);
                                    item = PROTECT(mkCharLen(str_ptr, str_array->size));
                                }
                                UNPROTECT(1);
                                SET_STRING_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                    case MORLOC_INT:
                        {
                            // Inline BigInt array: each element is [size:i64, value_or_relptr:i64].
                            // First pass: validate each element is single-limb AND fits 2^53.
                            // Multi-limb or beyond-2^53 elements cannot be represented in R's
                            // numeric type and would silently corrupt if cast through double.
                            int all_fit_int32 = 1;
                            size_t elem_w = element_schema->width; // 16
                            if (array->size > 0) {
                                start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                                for (size_t i = 0; i < array->size; i++) {
                                    const int64_t* f = (const int64_t*)(start + i * elem_w);
                                    int64_t bigint_size = f[0];
                                    if (bigint_size > 1) {
                                        MORLOC_ERROR(
                                            "Integer overflow at array index %zu: %lld-limb BigInt"
                                            " does not fit in R's numeric type"
                                            " (max 2^53 = 9007199254740992 for integer precision).",
                                            i, (long long)bigint_size);
                                    }
                                    int64_t val = (bigint_size == 0) ? 0 : f[1];
                                    if (val < INT32_MIN || val > INT32_MAX) all_fit_int32 = 0;
                                    if (val < -R_DOUBLE_INT_MAX || val > R_DOUBLE_INT_MAX) {
                                        MORLOC_ERROR(
                                            "Integer overflow at array index %zu: BigInt value %lld"
                                            " does not fit in R's numeric type"
                                            " (max 2^53 = 9007199254740992 for integer precision).",
                                            i, (long long)val);
                                    }
                                }
                            }
                            if (all_fit_int32) {
                                obj = PROTECT(allocVector(INTSXP, array->size));
                                if (array->size > 0) {
                                    for (size_t i = 0; i < array->size; i++) {
                                        const int64_t* f = (const int64_t*)(start + i * elem_w);
                                        INTEGER(obj)[i] = (int)((f[0] == 0) ? 0 : f[1]);
                                    }
                                }
                            } else {
                                obj = PROTECT(allocVector(REALSXP, array->size));
                                if (array->size > 0) {
                                    for (size_t i = 0; i < array->size; i++) {
                                        const int64_t* f = (const int64_t*)(start + i * elem_w);
                                        REAL(obj)[i] = (double)((f[0] == 0) ? 0 : f[1]);
                                    }
                                }
                            }
                            UNPROTECT(1);
                        }
                        break;
                    default:
                        {
                            obj = PROTECT(allocVector(VECSXP, array->size));
                            if(array->size == 0) {
                                UNPROTECT(1);
                                break;
                            }
                            start = (char*)R_TRY(resolve_relptr, array->data, base_ptr);
                            size_t width = element_schema->width;
                            for (size_t i = 0; i < array->size; i++) {
                                SEXP item = from_voidstar(start + width * i, element_schema, base_ptr);
                                if (item == R_NilValue) {
                                    UNPROTECT(1);
                                    obj = R_NilValue;
                                    goto error;
                                }
                                SET_VECTOR_ELT(obj, i, item);
                            }
                            UNPROTECT(1);
                        }
                        break;
                }
            }
            break;
        case MORLOC_TUPLE: {
            obj = PROTECT(allocVector(VECSXP, schema->size));
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                SEXP item = from_voidstar(item_ptr, schema->parameters[i], base_ptr);
                if (item == R_NilValue) {
                    UNPROTECT(1);
                    obj = R_NilValue;
                    goto error;
                }
                SET_VECTOR_ELT(obj, i, item);
            }
            UNPROTECT(1);
            break;
        }
        case MORLOC_MAP: {
            // R_NilValue is a legitimate field value: it stands for both
            // MORLOC_NIL fields and absent MORLOC_OPTIONAL fields (the
            // recursive `?T` tail of an LL, for instance). Real errors
            // inside from_voidstar take the R error() longjmp path and
            // never return, so reaching the return at all means success.
            obj = PROTECT(allocVector(VECSXP, schema->size));
            SEXP names = PROTECT(allocVector(STRSXP, schema->size));
            for (size_t i = 0; i < schema->size; i++) {
                void* item_ptr = (char*)data + schema->offsets[i];
                SEXP value = from_voidstar(item_ptr, schema->parameters[i], base_ptr);
                SET_VECTOR_ELT(obj, i, value);
                SET_STRING_ELT(names, i, mkChar(schema->keys[i]));
            }
            setAttrib(obj, R_NamesSymbol, names);
            UNPROTECT(2);
            break;
        }
        case MORLOC_OPTIONAL: {
            // The Optional slot is a relptr (RELNULL = absent). Resolve and
            // recurse into the inner T's body when present.
            relptr_t relptr = *(const relptr_t*)data;
            if (relptr == RELNULL) {
                return R_NilValue;
            }
            const void* inner_abs;
            if (base_ptr) {
                inner_abs = (const char*)base_ptr + relptr;
            } else {
                char* rel_err = NULL;
                inner_abs = rel2abs(relptr, &rel_err);
                if (rel_err) { free(rel_err); MORLOC_ERROR("rel2abs failed in MORLOC_OPTIONAL"); }
            }
            obj = from_voidstar(inner_abs, schema->parameters[0], base_ptr);
            break;
        }
        case MORLOC_RECUR: {
            const Schema* target = recur_env_lookup(schema->name);
            if (target == NULL) {
                MORLOC_ERROR("Recur back-reference to undeclared schema name '%s'",
                             schema->name ? schema->name : "?");
                goto error;
            }
            // Dispatch directly to _inner so the Recur node does not push
            // its own (lookup-key) name onto the env stack.
            obj = from_voidstar_inner(data, target, base_ptr);
            break;
        }
        default:
            MORLOC_ERROR("Unsupported schema type %d in from_voidstar", (int)schema->type);
            goto error;
    }

    return obj;

error:
    return R_NilValue;
}

// }}} from_voidstar

// {{{ exported morloc API functions

// PID of the process that created the daemon (set in morloc_start_daemon)
static pid_t daemon_creator_pid = 0;

// Close the daemon when the R object dies
static void daemon_finalizer(SEXP ptr) {
    if (!R_ExternalPtrAddr(ptr)) return;
    // Skip cleanup in forked children -- they must not unlink the socket file
    if (daemon_creator_pid != 0 && getpid() != daemon_creator_pid) {
        R_ClearExternalPtr(ptr);
        return;
    }
    language_daemon_t* daemon = (language_daemon_t*)R_ExternalPtrAddr(ptr);
    if(daemon != NULL){
        close_daemon(&daemon);
    }
    R_ClearExternalPtr(ptr);
}

// Release daemon resources in a forked child WITHOUT unlinking the socket file.
// Workers call this after fork so they don't hold the server_fd or accidentally
// destroy the socket when they exit.
SEXP morloc_detach_daemon(SEXP daemon_r) {
    if (!R_ExternalPtrAddr(daemon_r)) return R_NilValue;
    language_daemon_t* daemon = (language_daemon_t*)R_ExternalPtrAddr(daemon_r);
    if (daemon != NULL) {
        close_socket(daemon->server_fd);
        client_list_t *current = daemon->client_fds;
        while (current) {
            client_list_t *next = current->next;
            close(current->fd);
            free(current);
            current = next;
        }
        free(daemon->socket_path);
        free(daemon->tmpdir);
        free(daemon->shm_basename);
        free(daemon);
    }
    R_ClearExternalPtr(daemon_r);
    return R_NilValue;
}

SEXP morloc_start_daemon(
    SEXP socket_path_r,
    SEXP tmpdir_r,
    SEXP shm_basename_r,
    SEXP shm_default_size_r
){ MAYFAIL
    const char* socket_path = CHAR(STRING_ELT(socket_path_r, 0));
    const char* tmpdir = CHAR(STRING_ELT(tmpdir_r, 0));
    const char* shm_basename = CHAR(STRING_ELT(shm_basename_r, 0));
    size_t shm_default_size = (size_t)asInteger(shm_default_size_r);

    language_daemon_t* daemon = R_TRY(
        start_daemon,
        socket_path,
        tmpdir,
        shm_basename,
        shm_default_size
    );

    // Wrap pointer in external pointer
    SEXP result = PROTECT(R_MakeExternalPtr(daemon, R_NilValue, R_NilValue));

    // Record which process owns the daemon (for the PID guard in daemon_finalizer)
    daemon_creator_pid = getpid();

    // Register finalizer with wrapper
    R_RegisterCFinalizerEx(result, daemon_finalizer, TRUE);

    // Set class attribute
    SEXP class_name = PROTECT(mkString("language_daemon"));
    SET_CLASS(result, class_name);

    UNPROTECT(2);
    return result;
}



SEXP morloc_shinit(SEXP shm_basename_r, SEXP volume_index_r, SEXP shm_size_r) { MAYFAIL
    const char* shm_basename = CHAR(STRING_ELT(shm_basename_r, 0));
    size_t volume_index = (size_t)asInteger(volume_index_r);
    size_t shm_size = (size_t)asInteger(shm_size_r);

    R_TRY(shinit, shm_basename, volume_index, shm_size);

    return R_NilValue;
}


// {{{ signal handling for graceful shutdown

static volatile sig_atomic_t r_shutting_down = 0;

static void r_sigterm_handler(int sig) {
    (void)sig;
    r_shutting_down = 1;
}

SEXP morloc_install_sigterm_handler(void) {
    struct sigaction sa;
    sa.sa_handler = r_sigterm_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGTERM, &sa, NULL);

    /* Request SIGTERM when the parent (nexus) dies. Without this,
       SIGKILL on the nexus leaves pool processes orphaned with
       leaked SHM segments in /dev/shm. */
#ifdef __linux__
    prctl(PR_SET_PDEATHSIG, SIGTERM);
#endif

    return R_NilValue;
}

SEXP morloc_is_shutting_down(void) {
    return ScalarLogical(r_shutting_down != 0);
}

SEXP morloc_set_line_buffered(void) {
    // Only stderr - stdout is left fully buffered for performance
    // and flushed explicitly after each job.
    setvbuf(stderr, NULL, _IOLBF, 0);
    return R_NilValue;
}

// }}} signal handling

SEXP morloc_wait_for_client(SEXP daemon_r){ MAYFAIL
    if (!R_ExternalPtrAddr(daemon_r)) {
        MORLOC_ERROR("Expected a daemon pointer");
    }

    // Return immediately if shutdown was requested
    if (r_shutting_down) {
        return ScalarInteger(-1);
    }

    language_daemon_t* daemon = (language_daemon_t*)R_ExternalPtrAddr(daemon_r);

    // Use pselect directly (not wait_for_client_with_timeout) so we can
    // return immediately on EINTR from SIGTERM instead of retrying via WAIT
    fd_set read_fds;
    FD_ZERO(&read_fds);
    FD_SET(daemon->server_fd, &read_fds);
    int max_fd = daemon->server_fd;

    for (client_list_t* cl = daemon->client_fds; cl != NULL; cl = cl->next) {
        FD_SET(cl->fd, &read_fds);
        if (cl->fd > max_fd) max_fd = cl->fd;
    }

    // 100ms timeout -- short enough for responsive SIGTERM handling
    struct timespec ts = { .tv_sec = 0, .tv_nsec = 100000000 };
    sigset_t emptymask;
    sigemptyset(&emptymask);

    int ready = pselect(max_fd + 1, &read_fds, NULL, NULL, &ts, &emptymask);

    // Check shutdown after pselect (signal may have arrived during the call)
    if (r_shutting_down) {
        return ScalarInteger(-1);
    }

    // Timeout or interrupted -- return 0 (no client)
    if (ready <= 0) {
        return ScalarInteger(0);
    }

    // Accept new connection if server_fd is ready
    if (FD_ISSET(daemon->server_fd, &read_fds)) {
        int fd = accept(daemon->server_fd, NULL, NULL);
        if (fd >= 0) {
            fcntl(fd, F_SETFL, O_NONBLOCK);
            client_list_t* new_client = (client_list_t*)calloc(1, sizeof(client_list_t));
            if (new_client == NULL) {
                close(fd);
                MORLOC_ERROR("calloc failed");
            }
            new_client->fd = fd;
            new_client->next = NULL;
            if (daemon->client_fds == NULL) {
                daemon->client_fds = new_client;
            } else {
                client_list_t* last = daemon->client_fds;
                while (last->next) last = last->next;
                last->next = new_client;
            }
        }
    }

    // Return first ready client fd
    if (daemon->client_fds != NULL) {
        client_list_t* first = daemon->client_fds;
        int client_fd = first->fd;
        daemon->client_fds = first->next;
        free(first);
        return ScalarInteger(client_fd);
    }

    return ScalarInteger(0);
}


SEXP morloc_read_morloc_call_packet(SEXP packet_r) { MAYFAIL
    uint8_t* packet = RAW(packet_r);
    morloc_call_t* call_packet = R_TRY(read_morloc_call_packet, packet);

    // Create two element R list
    //  1: manifold id
    //  2: argument list of raw packets
    SEXP r_list = PROTECT(allocVector(VECSXP, 2));

    // Convert midx to R integer
    SEXP r_mid = PROTECT(ScalarInteger(call_packet->midx));

    // Create arguments list
    SEXP r_args = PROTECT(allocVector(VECSXP, call_packet->nargs));

    for(size_t i = 0; i < call_packet->nargs; i++) {
        size_t arg_packet_size = R_TRY_WITH(UNPROTECT(3), morloc_packet_size, call_packet->args[i]);
        SEXP r_arg = PROTECT(allocVector(RAWSXP, arg_packet_size));
        memcpy(RAW(r_arg), call_packet->args[i], arg_packet_size);
        SET_VECTOR_ELT(r_args, i, r_arg);
        UNPROTECT(1);  // r_arg
    }

    // Assemble final list
    SET_VECTOR_ELT(r_list, 0, r_mid);
    SET_VECTOR_ELT(r_list, 1, r_args);

    free_morloc_call(call_packet);

    UNPROTECT(3);  // r_list, r_mid, r_args
    return r_list;
}


SEXP morloc_send_packet_to_foreign_server(SEXP client_fd_r, SEXP packet_r) { MAYFAIL
    if (TYPEOF(client_fd_r) != INTSXP || LENGTH(client_fd_r) != 1) {
        MORLOC_ERROR("client_fd must be a single integer");
    }
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }

    // Extract arguments
    int client_fd = INTEGER(client_fd_r)[0];
    uint8_t* packet = RAW(packet_r);
    size_t packet_size = (size_t)LENGTH(packet_r);

    // Call underlying implementation
    size_t bytes_sent = R_TRY(send_packet_to_foreign_server, client_fd, packet);

    // This could in theory be problematic, since int is smaller than size_t
    // In practice it should not be, since packets are typically small
    // However, if I refactor to send large packets in the future, this could be
    // problematic. Then I would need to convert to a double return.
    return ScalarInteger((int)bytes_sent);
}


// Read from socket returning raw vector of received data
SEXP morloc_stream_from_client(SEXP client_fd_r) { MAYFAIL
    if (TYPEOF(client_fd_r) != INTSXP || LENGTH(client_fd_r) != 1) {
        MORLOC_ERROR("client_fd must be a single integer");
    }

    int client_fd = INTEGER(client_fd_r)[0];

    // Read packet from socket
    uint8_t* packet = R_TRY(stream_from_client, client_fd);

    // Read the packet size from the header (free packet before longjmp on error)
    size_t packet_size = R_TRY_WITH(free(packet), morloc_packet_size, packet);

    // Create raw vector for result
    SEXP result = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result), packet, packet_size);
    free(packet);

    UNPROTECT(1);
    return result;
}


// close_socket
SEXP morloc_close_socket(SEXP socket_id_r) {
    if (TYPEOF(socket_id_r) != INTSXP || LENGTH(socket_id_r) != 1) {
        MORLOC_ERROR("socket_id must be a single integer");
    }
    int socket_id = INTEGER(socket_id_r)[0];
    close_socket(socket_id);
    // Return invisible NULL
    return R_NilValue;
}


// put_value
SEXP morloc_put_value(SEXP obj_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("schema must be a single string");
    }

    const char* schema_cstr = CHAR(STRING_ELT(schema_str_r, 0));

    char* schema_str = strdup(schema_cstr);
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    // Arrow dispatch: schema marker `T` (MORLOC_TABLE) routes through the
    // Arrow C Data Interface. The legacy `<arrow>` hint has been retired;
    // the schema type itself now signals the dispatch.
    if (schema->type == MORLOC_TABLE) {
        // Export R arrow RecordBatch via C Data Interface -> copy to shm -> packet
        // arrow::ExportRecordBatch(batch, array_ptr, schema_ptr)
        struct ArrowSchema arrow_schema;
        struct ArrowArray arrow_array;
        memset(&arrow_schema, 0, sizeof(arrow_schema));
        memset(&arrow_array, 0, sizeof(arrow_array));

        SEXP arrow_ns = PROTECT(R_FindNamespace(mkString("arrow")));
        SEXP export_fn = PROTECT(findVarInFrame(arrow_ns, install("ExportRecordBatch")));
        if (export_fn == R_UnboundValue) {
            UNPROTECT(2);
            free_schema(schema);
            MORLOC_ERROR("arrow::ExportRecordBatch not found; is the arrow package installed?");
        }

        SEXP array_ptr_r = PROTECT(R_MakeExternalPtr(&arrow_array, R_NilValue, R_NilValue));
        SEXP schema_ptr_r = PROTECT(R_MakeExternalPtr(&arrow_schema, R_NilValue, R_NilValue));
        SEXP call = PROTECT(lang4(export_fn, obj_r, array_ptr_r, schema_ptr_r));
        eval(call, arrow_ns);
        UNPROTECT(5);

        char* errmsg = NULL;
        relptr_t relptr = arrow_to_shm(&arrow_array, &arrow_schema, &errmsg);

        if (arrow_schema.release) arrow_schema.release(&arrow_schema);
        if (arrow_array.release) arrow_array.release(&arrow_array);

        if (errmsg) {
            free_schema(schema);
            MORLOC_ERROR("Arrow export failed: %s", errmsg);
        }

        uint8_t* packet = make_arrow_data_packet(relptr, schema);
        if (!packet) {
            free_schema(schema);
            MORLOC_ERROR("Failed to create arrow data packet");
        }

        size_t packet_size = R_TRY_WITH({free(packet); free_schema(schema);}, morloc_packet_size, packet);
        SEXP result = PROTECT(allocVector(RAWSXP, packet_size));
        memcpy(RAW(result), packet, packet_size);
        free(packet);
        free_schema(schema);
        UNPROTECT(1);
        return result;
    }

    void* voidstar = to_voidstar(obj_r, schema);
    if (!voidstar) {
        free_schema(schema);
        MORLOC_ERROR("Failed to convert R object to internal representation");
    }

    relptr_t relptr = R_TRY_WITH(free_schema(schema), abs2rel, voidstar);

    uint8_t* packet = R_TRY_WITH(free_schema(schema), make_data_packet_auto, voidstar, relptr, schema);

    const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
    bool tracked = false;
    if (hdr->command.data.source == PACKET_SOURCE_RPTR) {
        // SHM is referenced by the result packet; the pool retains the only
        // reference until the next request. Hand voidstar AND schema to the
        // tracker -- they are freed by shm_tracker_flush at the start of the
        // next dispatch in run_job_c. Without this, every RPTR-shipped
        // value would leak its SHM block for the lifetime of the pool.
        shm_tracker_push((absptr_t)voidstar, schema);
        tracked = true;
    } else {
        // Data inlined in packet -- free SHM immediately. shfree zeros the
        // block on final ref-drop.
        char* free_err = NULL;
        shfree((absptr_t)voidstar, &free_err);
        if (free_err) { free(free_err); }
    }

    size_t packet_size = R_TRY_WITH(
        {free(packet); if (!tracked) free_schema(schema);},
        morloc_packet_size, packet);

    SEXP result = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result), packet, packet_size);
    free(packet);
    if (!tracked) {
        free_schema(schema);
    }

    UNPROTECT(1);
    return result;
}


// ── Stream-handle bindings ───────────────────────────────────────────────

// @open: returns a 64-bit handle. R has no native int64 SEXP type, so
// we marshal via a length-1 numeric vector (double) which preserves
// the 48 generation bits + 16 slot bits without precision loss for any
// realistic morloc program.
SEXP morloc_mlc_open(SEXP path_r, SEXP kind_r) { MAYFAIL
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_open: path must be a single string");
    }
    if ((TYPEOF(kind_r) != INTSXP && TYPEOF(kind_r) != REALSXP) || LENGTH(kind_r) != 1) {
        MORLOC_ERROR("mlc_open: kind must be a single integer");
    }
    const char* path = CHAR(STRING_ELT(path_r, 0));
    int kind_i = (TYPEOF(kind_r) == INTSXP) ? INTEGER(kind_r)[0] : (int)REAL(kind_r)[0];
    int64_t handle = R_TRY(mlc_open, path, (uint8_t)kind_i);
    // IFile handles use the full UInt64 range (48 generation bits + 16
    // slot bits + per-process salt). Returning as bit64::integer64
    // preserves the bit pattern losslessly across R user code, which
    // would otherwise silently round high generations to nearby doubles.
    return make_integer64_scalar(handle);
}

SEXP morloc_mlc_close(SEXP handle_r) { MAYFAIL
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP) || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_close: handle must be a single number");
    }
    int64_t handle = i64_from_sexp(handle_r);
    R_TRY(mlc_close, handle);
    return R_NilValue;
}

SEXP morloc_mlc_fschema(SEXP path_r) { MAYFAIL
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_fschema: path must be a single string");
    }
    const char* path = CHAR(STRING_ELT(path_r, 0));
    char* s = R_TRY(mlc_fschema, path);
    if (s == NULL) {
        MORLOC_ERROR("mlc_fschema returned NULL");
    }
    SEXP result = PROTECT(mkString(s));
    free(s);
    UNPROTECT(1);
    return result;
}

// Shared scaffold for the @save family. The three formats (voidstar,
// msgpack, JSON) share the same R-side shape: pack value -> SHM voidstar,
// hand off to the corresponding libmorloc save function, free the SHM
// block. Each wrapper passes the libmorloc function pointer for the
// format-specific write.
typedef int (*morloc_save_fn)(const absptr_t, const Schema*, uint8_t,
                              const char*, char**);

static SEXP morloc_mlc_save_dispatch(SEXP obj_r, SEXP schema_str_r,
                                     SEXP level_r, SEXP path_r,
                                     morloc_save_fn save_fn,
                                     const char* fn_name) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("%s: schema must be a single string", fn_name);
    }
    if (!(TYPEOF(level_r) == INTSXP || TYPEOF(level_r) == REALSXP)
        || LENGTH(level_r) != 1) {
        MORLOC_ERROR("%s: level must be a single integer 0-9", fn_name);
    }
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("%s: path must be a single string", fn_name);
    }
    uint8_t level = (uint8_t)asInteger(level_r);
    const char* path = CHAR(STRING_ELT(path_r, 0));

    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    void* voidstar = to_voidstar(obj_r, schema);
    if (!voidstar) {
        free_schema(schema);
        MORLOC_ERROR("%s: failed to convert R object to voidstar", fn_name);
    }

    R_TRY_WITH(
        { char* err = NULL; shfree(voidstar, &err); free(err); free_schema(schema); },
        save_fn, (absptr_t)voidstar, schema, level, path);

    {
        char* err = NULL;
        shfree(voidstar, &err);
        free(err);
    }
    free_schema(schema);
    return R_NilValue;
}

// @save :: voidstar binary file format. `level` is the zstd preset
// (0 = uncompressed, 1-9 = increasing ratio).
SEXP morloc_mlc_save_voidstar(SEXP obj_r, SEXP schema_str_r,
                              SEXP level_r, SEXP path_r) {
    return morloc_mlc_save_dispatch(obj_r, schema_str_r, level_r, path_r,
                                    mlc_save_voidstar, "mlc_save_voidstar");
}

// @savem :: msgpack file format. The runtime ignores `level` for the
// msgpack path (no packet header in which to record compression), but
// the ABI keeps it for uniformity with the voidstar family.
SEXP morloc_mlc_save(SEXP obj_r, SEXP schema_str_r,
                    SEXP level_r, SEXP path_r) {
    return morloc_mlc_save_dispatch(obj_r, schema_str_r, level_r, path_r,
                                    mlc_save, "mlc_save");
}

// @savej :: JSON file format. `level` is currently ignored.
SEXP morloc_mlc_save_json(SEXP obj_r, SEXP schema_str_r,
                         SEXP level_r, SEXP path_r) {
    return morloc_mlc_save_dispatch(obj_r, schema_str_r, level_r, path_r,
                                    mlc_save_json, "mlc_save_json");
}

// @hash :: content-addressed digest of a value. Returns a hex string.
// Mirrors pymorloc.c's pybinding__mlc_hash / pool.cpp's _mlc_hash.
SEXP morloc_mlc_hash(SEXP obj_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_hash: schema must be a single string");
    }
    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    void* voidstar = to_voidstar(obj_r, schema);
    if (!voidstar) {
        free_schema(schema);
        MORLOC_ERROR("mlc_hash: failed to convert R object to voidstar");
    }

    char* hex = R_TRY_WITH(
        { char* err = NULL; shfree(voidstar, &err); free(err); free_schema(schema); },
        mlc_hash, voidstar, schema);

    {
        char* err = NULL;
        shfree(voidstar, &err);
        free(err);
    }
    free_schema(schema);

    SEXP result = PROTECT(mkString(hex));
    free(hex);
    UNPROTECT(1);
    return result;
}

// @load :: load a value from a file (auto-detects voidstar / msgpack
// based on the file's header). Returns the decoded value, or NULL on
// IO / parse error. Mirrors pymorloc.c's pybinding__mlc_load.
SEXP morloc_mlc_load(SEXP schema_str_r, SEXP path_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_load: schema must be a single string");
    }
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_load: path must be a single string");
    }
    const char* path = CHAR(STRING_ELT(path_r, 0));

    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    void* voidstar = R_TRY_WITH(free_schema(schema), mlc_load, path, schema);
    if (voidstar == NULL) {
        // Clean parse/IO failure with no errmsg -> surface as R NULL so
        // morloc's <Maybe>-typed @load arm can pattern-match.
        free_schema(schema);
        return R_NilValue;
    }

    SEXP obj = from_voidstar(voidstar, schema, NULL);
    // shm_tracker matches the pymorloc / cppmorloc deferred-cleanup
    // pattern: defer the shfree until the next request so any
    // R-side view that points at the SHM block stays valid for the
    // current call's lifetime.
    shm_tracker_push((absptr_t)voidstar, schema);
    // ownership of `schema` is transferred to shm_tracker; do NOT
    // free_schema here.
    return obj;
}

// @read :: parse a JSON string into a value. Returns the decoded value,
// or NULL on parse error. Mirrors pymorloc.c's pybinding__mlc_read.
SEXP morloc_mlc_read(SEXP schema_str_r, SEXP json_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_read: schema must be a single string");
    }
    if (TYPEOF(json_str_r) != STRSXP || LENGTH(json_str_r) != 1) {
        MORLOC_ERROR("mlc_read: json must be a single string");
    }
    const char* json_str = CHAR(STRING_ELT(json_str_r, 0));

    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    // mlc_read may legitimately return NULL on parse failure even with
    // no errmsg set; the morloc <Maybe>-typed @read arm relies on that
    // to surface as R NULL.
    char* errmsg = NULL;
    void* voidstar = mlc_read(json_str, schema, &errmsg);
    if (errmsg != NULL) {
        free(errmsg);
    }
    if (voidstar == NULL) {
        free_schema(schema);
        return R_NilValue;
    }

    SEXP obj = from_voidstar(voidstar, schema, NULL);
    shm_tracker_push((absptr_t)voidstar, schema);
    return obj;
}

// Helper: R bound -> (has_X, value) pair for one mlc_ifile_walk arg.
// NULL / NA / NA_integer_ / NA_real_ -> absent (has=0); else use the
// integer / real / integer64 value.
static inline void r_optint_to_pair(SEXP x, uint8_t* has_out, int64_t* val_out) {
    *has_out = 1;
    *val_out = 0;
    if (x == R_NilValue) { *has_out = 0; return; }
    if (TYPEOF(x) == INTSXP) {
        if (LENGTH(x) == 0 || INTEGER(x)[0] == NA_INTEGER) { *has_out = 0; return; }
        *val_out = (int64_t)INTEGER(x)[0];
        return;
    }
    if (TYPEOF(x) == REALSXP) {
        if (LENGTH(x) == 0) { *has_out = 0; return; }
        if (is_integer64(x)) {
            // bit64::integer64: 8 bytes are the int64. NA_INTEGER64
            // is INT64_MIN; we surface NA as has=0 to match the
            // optional-bound semantics callers expect.
            int64_t v;
            memcpy(&v, &REAL(x)[0], sizeof(int64_t));
            if (v == INT64_MIN) { *has_out = 0; return; }
            *val_out = v;
            return;
        }
        if (ISNA(REAL(x)[0])) { *has_out = 0; return; }
        *val_out = (int64_t)REAL(x)[0];
        return;
    }
    *has_out = 0;
}

// Unified IFile pattern walker. R signature:
//   morloc_mlc_ifile_walk(schema_str, handle, path, args_list)
// where `args_list` is an R list of (NULL | integer | numeric), one per
// bracket runtime arg in DFS order (field steps consume no args;
// bracket-index consumes 1; bracket-slice consumes 3).
SEXP morloc_mlc_ifile_walk(SEXP schema_str_r, SEXP handle_r,
                           SEXP path_r, SEXP args_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_ifile_walk: schema must be a single string");
    }
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP) || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_ifile_walk: handle must be a single number");
    }
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_ifile_walk: path must be a single string");
    }
    if (TYPEOF(args_r) != VECSXP) {
        MORLOC_ERROR("mlc_ifile_walk: args must be a list");
    }
    int64_t handle = i64_from_sexp(handle_r);
    const char* path = CHAR(STRING_ELT(path_r, 0));

    R_xlen_t n = XLENGTH(args_r);
    mlc_ifile_walk_arg* packed = NULL;
    if (n > 0) {
        packed = (mlc_ifile_walk_arg*)calloc((size_t)n, sizeof(mlc_ifile_walk_arg));
        if (packed == NULL) MORLOC_ERROR("mlc_ifile_walk: alloc failed");
        for (R_xlen_t i = 0; i < n; i++) {
            r_optint_to_pair(VECTOR_ELT(args_r, i), &packed[i].has, &packed[i].value);
        }
    }

    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(do { free(schema_str); free(packed); } while(0),
                                parse_schema, schema_str);
    free(schema_str);

    void* voidstar = R_TRY_WITH(do { free_schema(schema); free(packed); } while(0),
                                mlc_ifile_walk,
                                handle, path, packed, (uint64_t)n);
    free(packed);
    if (voidstar == NULL) {
        free_schema(schema);
        return R_NilValue;
    }
    SEXP result = from_voidstar(voidstar, schema, NULL);
    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return result;
}

SEXP morloc_mlc_ifile_length(SEXP handle_r) { MAYFAIL
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP) || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_ifile_length: handle must be a single number");
    }
    int64_t handle = i64_from_sexp(handle_r);
    int64_t n = R_TRY(mlc_ifile_length, handle);
    // length returns Int (morloc); on the R side Int maps to "integer"
    // (32-bit) -- which would overflow for very large lengths. Use
    // numeric/double here for compatibility with the existing R Int
    // pack/unpack path (Int < 2^53 round-trips losslessly). Callers
    // who need the full 64-bit range can switch to mlc_ifile_walk's
    // path-string + arg list directly.
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)n;
    UNPROTECT(1);
    return result;
}

// @next: materialise the IStream's current sub-packet into R-side
// `[a]`, advancing the cursor. Mirrors mlc_ifile_walk's shfree-deferred
// pattern so from_voidstar can produce zero-copy views.
SEXP morloc_mlc_next(SEXP schema_str_r, SEXP handle_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_next: schema must be a single string");
    }
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP) || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_next: handle must be a single number");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    int64_t handle = i64_from_sexp(handle_r);
    Schema* schema = R_TRY(parse_schema, schema_str);
    void* voidstar = R_TRY(mlc_next, handle);
    if (voidstar == NULL) {
        free_schema(schema);
        return R_NilValue;
    }
    SEXP result = from_voidstar(voidstar, schema, NULL);
    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return result;
}

SEXP morloc_mlc_stream(SEXP ifile_handle_r) { MAYFAIL
    if ((TYPEOF(ifile_handle_r) != INTSXP && TYPEOF(ifile_handle_r) != REALSXP)
        || LENGTH(ifile_handle_r) != 1) {
        MORLOC_ERROR("mlc_stream: ifile handle must be a single number");
    }
    int64_t ifh = i64_from_sexp(ifile_handle_r);
    int64_t new_h = R_TRY(mlc_stream, ifh);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)new_h;
    UNPROTECT(1);
    return result;
}

// @open path :: <IO> (OStream T) -- typed open binding.
SEXP morloc_mlc_open_ostream(SEXP schema_str_r, SEXP path_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_open_ostream: schema must be a single string");
    }
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_open_ostream: path must be a single string");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    const char* path = CHAR(STRING_ELT(path_r, 0));
    int64_t h = R_TRY(mlc_open_ostream, schema_str, path);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)h;
    UNPROTECT(1);
    return result;
}

// @stdin / @stdout / @stderr :: <IO> Handle T -- nullary intrinsics.
// Nexus owns fd 0/1/2; the runtime routes @next / @write through the
// pool-nexus RPC socket.
SEXP morloc_mlc_open_stdin(SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_open_stdin: schema must be a single string");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    int64_t h = R_TRY(mlc_open_stdin, schema_str);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)h;
    UNPROTECT(1);
    return result;
}

SEXP morloc_mlc_open_stdout(SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_open_stdout: schema must be a single string");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    int64_t h = R_TRY(mlc_open_stdout, schema_str);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)h;
    UNPROTECT(1);
    return result;
}

SEXP morloc_mlc_open_stderr(SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_open_stderr: schema must be a single string");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    int64_t h = R_TRY(mlc_open_stderr, schema_str);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)h;
    UNPROTECT(1);
    return result;
}

// @write: serialise value to voidstar, emit one sub-packet.
SEXP morloc_mlc_write(SEXP schema_str_r, SEXP level_r, SEXP value_r, SEXP handle_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_write: schema must be a single string");
    }
    if ((TYPEOF(level_r) != INTSXP && TYPEOF(level_r) != REALSXP)
        || LENGTH(level_r) != 1) {
        MORLOC_ERROR("mlc_write: level must be a single number");
    }
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP)
        || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_write: handle must be a single number");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    int64_t handle = i64_from_sexp(handle_r);
    uint8_t level = (uint8_t)i64_from_sexp(level_r);
    Schema* schema = R_TRY(parse_schema, schema_str);
    size_t bytes = get_shm_size(schema, value_r);
    void* voidstar = R_TRY(shmalloc, bytes);
    void* cursor = (uint8_t*)voidstar + schema->width;
    to_voidstar_inner(voidstar, &cursor, value_r, schema);
    R_TRY(mlc_write, level, handle, voidstar);
    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);
    return R_NilValue;
}

SEXP morloc_mlc_append(SEXP schema_str_r, SEXP path_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("mlc_append: schema must be a single string");
    }
    if (TYPEOF(path_r) != STRSXP || LENGTH(path_r) != 1) {
        MORLOC_ERROR("mlc_append: path must be a single string");
    }
    const char* schema_str = CHAR(STRING_ELT(schema_str_r, 0));
    const char* path = CHAR(STRING_ELT(path_r, 0));
    int64_t h = R_TRY(mlc_append, schema_str, path);
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = (double)h;
    UNPROTECT(1);
    return result;
}

SEXP morloc_mlc_concat(SEXP paths_r, SEXP dest_r) { MAYFAIL
    if (TYPEOF(paths_r) != STRSXP) {
        MORLOC_ERROR("mlc_concat: paths must be a character vector");
    }
    if (TYPEOF(dest_r) != STRSXP || LENGTH(dest_r) != 1) {
        MORLOC_ERROR("mlc_concat: dest must be a single string");
    }
    R_xlen_t n = XLENGTH(paths_r);
    const char** raw = NULL;
    if (n > 0) {
        raw = (const char**)R_alloc((size_t)n, sizeof(char*));
        for (R_xlen_t i = 0; i < n; i++) {
            raw[i] = CHAR(STRING_ELT(paths_r, i));
        }
    }
    R_TRY(mlc_concat, raw, (size_t)n, CHAR(STRING_ELT(dest_r, 0)));
    return R_NilValue;
}

SEXP morloc_mlc_flush(SEXP handle_r) { MAYFAIL
    if ((TYPEOF(handle_r) != INTSXP && TYPEOF(handle_r) != REALSXP) || LENGTH(handle_r) != 1) {
        MORLOC_ERROR("mlc_flush: handle must be a single number");
    }
    int64_t handle = i64_from_sexp(handle_r);
    R_TRY(mlc_flush, handle);
    return R_NilValue;
}


// mlc_show: serialize a value to a JSON string
SEXP morloc_mlc_show(SEXP obj_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("schema must be a single string");
    }

    char* schema_str = strdup(CHAR(STRING_ELT(schema_str_r, 0)));
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    void* voidstar = to_voidstar(obj_r, schema);
    if (!voidstar) {
        free_schema(schema);
        MORLOC_ERROR("Failed to convert R object to internal representation");
    }

    char* json = R_TRY_WITH(free_schema(schema), mlc_show, voidstar, schema);

    {
        char* shfree_errmsg = NULL;
        shfree(voidstar, &shfree_errmsg);
        free(shfree_errmsg);
    }
    free_schema(schema);

    SEXP result = PROTECT(mkString(json));
    free(json);
    UNPROTECT(1);
    return result;
}


SEXP morloc_get_value(SEXP packet_r, SEXP schema_str_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }
    if (TYPEOF(schema_str_r) != STRSXP || LENGTH(schema_str_r) != 1) {
        MORLOC_ERROR("schema must be a single string");
    }

    // Extract arguments
    uint8_t* packet = RAW(packet_r);
    size_t packet_size = (size_t)LENGTH(packet_r);

    const morloc_packet_header_t* header = (const morloc_packet_header_t*)packet;
    uint8_t source = header->command.data.source;
    uint8_t format = header->command.data.format;

    const char* schema_cstr = CHAR(STRING_ELT(schema_str_r, 0));

    char* schema_str = strdup(schema_cstr);
    Schema* schema = R_TRY_WITH(free(schema_str), parse_schema, schema_str);
    free(schema_str);

    // Arrow dispatch: if packet format is Arrow, import via C Data Interface
    if (format == PACKET_FORMAT_ARROW) {
        uint8_t* arrow_ptr = R_TRY_WITH(free_schema(schema),
            get_morloc_data_packet_value, packet, schema);
        const arrow_shm_header_t* arrow_hdr = (const arrow_shm_header_t*)arrow_ptr;

        struct ArrowSchema arrow_schema;
        struct ArrowArray arrow_array;
        char* arrow_err = NULL;
        arrow_from_shm(arrow_hdr, &arrow_schema, &arrow_array, &arrow_err);
        if (arrow_err) {
            if (arrow_schema.release) arrow_schema.release(&arrow_schema);
            if (arrow_array.release) arrow_array.release(&arrow_array);
            free_schema(schema);
            MORLOC_ERROR("Arrow import failed: %s", arrow_err);
        }

        // Import via R arrow package: arrow::ImportRecordBatch(array_ptr, schema_ptr)
        SEXP arrow_ns = PROTECT(R_FindNamespace(mkString("arrow")));
        SEXP import_fn = PROTECT(findVarInFrame(arrow_ns, install("ImportRecordBatch")));
        if (import_fn == R_UnboundValue) {
            if (arrow_schema.release) arrow_schema.release(&arrow_schema);
            if (arrow_array.release) arrow_array.release(&arrow_array);
            UNPROTECT(2);
            free_schema(schema);
            MORLOC_ERROR("arrow::ImportRecordBatch not found; is the arrow package installed?");
        }

        SEXP array_ptr_r = PROTECT(R_MakeExternalPtr(&arrow_array, R_NilValue, R_NilValue));
        SEXP schema_ptr_r = PROTECT(R_MakeExternalPtr(&arrow_schema, R_NilValue, R_NilValue));
        SEXP call = PROTECT(lang3(import_fn, array_ptr_r, schema_ptr_r));
        SEXP obj_r = PROTECT(eval(call, arrow_ns));
        UNPROTECT(6);

        // Incref shm so data stays alive
        char* incref_err = NULL;
        shincref((absptr_t)arrow_ptr, &incref_err);
        if (incref_err) { free(incref_err); }

        free_schema(schema);
        return obj_r;
    }

    // Fast path: inline voidstar -- read directly from packet, no SHM needed
    if (source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_VOIDSTAR) {
        const uint8_t* payload = packet + sizeof(morloc_packet_header_t) + header->offset;
        SEXP obj_r = from_voidstar((const void*)payload, schema, (const void*)payload);
        free_schema(schema);
        if (obj_r == NULL) {
            MORLOC_ERROR("Failed to convert internal representation to R object");
        }
        return obj_r;
    }

    // Stream ingest: FILE+DATA pointing at a stream-packet file. Nexus
    // emits this shape for `[a]` receivers; peek here to distinguish
    // stream from ordinary data-indirection. Uses `mlc_ifile_length`
    // to preallocate the result vector (element_count from the file's
    // StreamDiag), so PROTECT depth stays at O(1) regardless of how
    // many sub-packets the stream carries.
    if (source == PACKET_SOURCE_FILE && format == PACKET_FORMAT_DATA) {
        size_t payload_len = header->length;
        char path[PATH_MAX];
        if (payload_len >= sizeof(path)) {
            free_schema(schema);
            MORLOC_ERROR("stream ingest: path exceeds PATH_MAX");
        }
        memcpy(path, packet + sizeof(morloc_packet_header_t) + header->offset,
               payload_len);
        path[payload_len] = '\0';
        if (file_is_stream_packet(path)) {
            char* open_err = NULL;
            int64_t handle = mlc_open(path, MLC_KIND_ISTREAM, &open_err);
            if (open_err) {
                char msg[512];
                snprintf(msg, sizeof(msg), "stream ingest: mlc_open failed: %s", open_err);
                free(open_err);
                free_schema(schema);
                MORLOC_ERROR("%s", msg);
            }
            char* len_err = NULL;
            int64_t total_i64 = mlc_ifile_length(handle, &len_err);
            if (len_err || total_i64 < 0) {
                char* cerr = NULL; mlc_close(handle, &cerr);
                if (cerr) free(cerr);
                free_schema(schema);
                if (len_err) {
                    char msg[512];
                    snprintf(msg, sizeof(msg), "stream ingest: mlc_ifile_length failed: %s", len_err);
                    free(len_err);
                    MORLOC_ERROR("%s", msg);
                }
                MORLOC_ERROR("stream ingest: mlc_ifile_length returned %lld",
                             (long long)total_i64);
            }
            SEXP result = PROTECT(allocVector(VECSXP, (R_xlen_t)total_i64));
            R_xlen_t off = 0;
            while (1) {
                char* nerr = NULL;
                void* chunk = mlc_next(handle, &nerr);
                if (nerr) {
                    UNPROTECT(1);
                    char msg[512];
                    snprintf(msg, sizeof(msg), "stream ingest: mlc_next failed: %s", nerr);
                    free(nerr);
                    char* cerr = NULL; mlc_close(handle, &cerr);
                    if (cerr) free(cerr);
                    free_schema(schema);
                    MORLOC_ERROR("%s", msg);
                }
                if (chunk == NULL) break;
                Array* arr = (Array*)chunk;
                if (arr->size == 0) {
                    char* ferr = NULL; shfree(chunk, &ferr);
                    if (ferr) free(ferr);
                    break;
                }
                SEXP chunk_r = PROTECT(from_voidstar(chunk, schema, NULL));
                char* ferr = NULL; shfree(chunk, &ferr);
                if (ferr) free(ferr);
                if (chunk_r == NULL) {
                    UNPROTECT(2);
                    char* cerr = NULL; mlc_close(handle, &cerr);
                    if (cerr) free(cerr);
                    free_schema(schema);
                    MORLOC_ERROR("stream ingest: from_voidstar failed on chunk");
                }
                R_xlen_t sz = XLENGTH(chunk_r);
                for (R_xlen_t j = 0; j < sz && off < (R_xlen_t)total_i64; j++) {
                    SET_VECTOR_ELT(result, off++, VECTOR_ELT(chunk_r, j));
                }
                UNPROTECT(1);
            }
            char* cerr = NULL; mlc_close(handle, &cerr);
            if (cerr) free(cerr);
            UNPROTECT(1);
            free_schema(schema);
            return result;
        }
        // Not a stream file -- fall through to legacy FILE+DATA handling.
    }

    // SHM paths (RPTR source from upstream pool/daemon, or our own
    // unpack_with_schema for MESG msgpack args).
    bool is_rptr = (source == PACKET_SOURCE_RPTR);
    uint8_t* voidstar = R_TRY_WITH(free_schema(schema), get_morloc_data_packet_value, packet, schema);

    if (is_rptr) {
        // Sender (daemon or peer pool) holds the original ref and will
        // release it at their next dispatch. We add our own ref and stash
        // it in the tracker so shm_tracker_flush() releases it at the
        // start of our next request -- after R has finished consuming
        // the deserialized form.
        char* incref_err = NULL;
        shincref((absptr_t)voidstar, &incref_err);
        if (incref_err) { free(incref_err); }
        shm_tracker_push((absptr_t)voidstar, schema);
    }

    SEXP obj_r = from_voidstar(voidstar, schema, NULL);
    if (obj_r == NULL) {
        if (!is_rptr) {
            char* free_err = NULL;
            shfree((absptr_t)voidstar, &free_err);
            if (free_err) { free(free_err); }
            free_schema(schema);
        }
        // For RPTR, schema and voidstar are owned by the tracker; do not
        // double-free here.
        MORLOC_ERROR("Failed to convert internal representation to R object");
    }

    if (!is_rptr) {
        // We allocated this voidstar (via unpack_with_schema for MESG
        // msgpack args). from_voidstar deep-copied into R-managed memory,
        // so the SHM block is no longer needed.
        char* free_err = NULL;
        shfree((absptr_t)voidstar, &free_err);
        if (free_err) { free(free_err); }
        free_schema(schema);
    }

    return obj_r;
}


SEXP morloc_foreign_call(SEXP socket_path_r, SEXP mid_r, SEXP args_r) { MAYFAIL
    // Validate inputs
    if (TYPEOF(socket_path_r) != STRSXP || LENGTH(socket_path_r) != 1) {
        MORLOC_ERROR("socket_path must be a single string");
    }
    if (TYPEOF(mid_r) != INTSXP || LENGTH(mid_r) != 1) {
        MORLOC_ERROR("mid must be a single integer");
    }
    if (TYPEOF(args_r) != VECSXP) {
        MORLOC_ERROR("args must be a list of raw vectors");
    }

    // Extract arguments
    const char* socket_path = CHAR(STRING_ELT(socket_path_r, 0));
    int mid = INTEGER(mid_r)[0];
    size_t nargs = (size_t)LENGTH(args_r);

    // Allocate temporary storage
    const uint8_t** arg_packets = (const uint8_t**)R_alloc(nargs, sizeof(uint8_t*));

    // Convert R raw vectors to C buffers
    for (size_t i = 0; i < nargs; i++) {
        SEXP arg = VECTOR_ELT(args_r, i);
        if (TYPEOF(arg) != RAWSXP) {
            MORLOC_ERROR("All arguments must be raw vectors (argument %zu)", i+1);
        }
        arg_packets[i] = RAW(arg);
    }

    // Create call packet
    uint8_t* packet = R_TRY(
        make_morloc_local_call_packet,
        (uint32_t)mid,
        arg_packets,
        nargs
    );

    // Send/receive over socket
    uint8_t* result = R_TRY_WITH(free(packet),
        send_and_receive_over_socket,
        socket_path,
        packet
    );

    // If the foreign pool returned a fail packet, surface it as an R
    // error. Without this the raw fail-packet bytes get returned to the
    // autogen caller which then tries to deserialize them as data,
    // producing a confusing downstream error or crashing the worker.
    {
        char* fail_check_err = NULL;
        char* fail_msg = get_morloc_data_packet_error_message(
            (const uint8_t*)result, &fail_check_err);
        if (fail_check_err != NULL) { free(fail_check_err); }
        if (fail_msg != NULL) {
            // Take ownership of fail_msg before longjmping out via error().
            // Rf_error longjmps to R's condition handler; the copy leaks on
            // that path (process is unwinding anyway) but the previous fixed
            // 8KB buffer silently truncated long tracebacks.
            char* msg_copy = strdup(fail_msg);
            free(fail_msg);
            free(packet);
            free(result);
            if (msg_copy == NULL) {
                error("morloc R foreign_call: OOM copying fail message");
            }
            error("%s", msg_copy);
        }
    }

    // Get result size
    size_t result_length = R_TRY_WITH({free(packet); free(result);}, morloc_packet_size, result);

    // Create result raw vector
    SEXP result_r = PROTECT(allocVector(RAWSXP, result_length));
    memcpy(RAW(result_r), result, result_length);
    free(packet);
    free(result);

    // Cleanup
    UNPROTECT(1);
    return result_r;
}


// ── log emission bridge to libmorloc.so ──────────────────────────────────

SEXP morloc_log_next_id_r(void) {
    uint64_t id = morloc_log_next_id();
    // R's numeric is double; for our pid:counter values this is safe up
    // to 2^53. The pool round-trips the id back into morloc_log_emit_r
    // without inspecting it.
    return ScalarReal((double)id);
}

SEXP morloc_log_emit_r(SEXP tmpl_r, SEXP group_r, SEXP runtime_r, SEXP call_id_r) {
    if (TYPEOF(tmpl_r) != STRSXP || LENGTH(tmpl_r) != 1) {
        MORLOC_ERROR("log_emit: template must be a single string");
    }
    const char* tmpl = CHAR(STRING_ELT(tmpl_r, 0));
    // group may be NA or an empty string -- both translate to a NULL
    // C pointer so the tee is skipped.
    const char* group = NULL;
    if (TYPEOF(group_r) == STRSXP && LENGTH(group_r) == 1) {
        SEXP s = STRING_ELT(group_r, 0);
        if (s != NA_STRING) {
            const char* g = CHAR(s);
            if (g && g[0] != '\0') {
                group = g;
            }
        }
    }
    double runtime = asReal(runtime_r);
    uint64_t call_id = (uint64_t)asReal(call_id_r);
    morloc_log_emit(tmpl, group, runtime, call_id);
    return R_NilValue;
}

SEXP morloc_is_ping(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }

    bool is_ping = R_TRY(packet_is_ping, RAW(packet_r));

    return ScalarLogical(is_ping);
}


SEXP morloc_is_local_call(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }

    bool is_local_call = R_TRY(packet_is_local_call, RAW(packet_r));

    return ScalarLogical(is_local_call);
}


SEXP morloc_is_remote_call(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }

    bool is_remote_call = R_TRY(packet_is_remote_call, RAW(packet_r));

    return ScalarLogical(is_remote_call);
}


SEXP morloc_pong(SEXP packet_r) { MAYFAIL
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("packet must be a raw vector");
    }

    // Generate a response to ping
    uint8_t* pong = R_TRY(return_ping, RAW(packet_r));

    size_t pong_size = R_TRY_WITH(free(pong), morloc_packet_size, pong);

    SEXP result_r = PROTECT(allocVector(RAWSXP, pong_size));
    memcpy(RAW(result_r), pong, pong_size);
    free(pong);

    UNPROTECT(1);
    return result_r;
}


SEXP morloc_make_fail_packet(SEXP failure_message_r) { MAYFAIL
    const char* failure_message = CHAR(STRING_ELT(failure_message_r, 0));
    uint8_t* fail_packet = make_fail_packet(failure_message);

    size_t packet_size = R_TRY(morloc_packet_size, fail_packet);

    SEXP packet_r = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(packet_r), fail_packet, packet_size);
    free(fail_packet);

    UNPROTECT(1);
    return packet_r;
}


// Release the SHM ref owned by a morloc_put_value-produced packet. The
// codegen inserts this call at the end of a serialize let's scope so
// the tracker entry is dropped as soon as the packet is no longer
// needed. No-op for inline (non-RPTR) packets, so callers can invoke
// unconditionally.
SEXP morloc_release_packet_shm(SEXP packet_r) {
    if (TYPEOF(packet_r) != RAWSXP) {
        return R_NilValue;
    }
    R_xlen_t packet_size = LENGTH(packet_r);
    if ((size_t)packet_size < sizeof(morloc_packet_header_t)) {
        return R_NilValue;
    }
    const uint8_t* packet = (const uint8_t*)RAW(packet_r);
    const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
    if (hdr->command.data.source != PACKET_SOURCE_RPTR) {
        return R_NilValue;
    }
    size_t relptr = *(size_t*)(packet
        + sizeof(morloc_packet_header_t) + hdr->offset);
    char* resolve_err = NULL;
    void* voidstar = rel2abs(relptr, &resolve_err);
    if (resolve_err) { free(resolve_err); resolve_err = NULL; }
    if (voidstar) {
        shm_tracker_release_one((absptr_t)voidstar);
    }
    return R_NilValue;
}


SEXP extract_element_by_name(SEXP list, const char* key) {
  // Ensure inputs are correct types
  if (TYPEOF(list) != VECSXP) MORLOC_ERROR("Input must be a list");

  // Get list names attribute
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  if (names == R_NilValue) MORLOC_ERROR("List must have names");

  // Iterate through list elements
  for (int i = 0; i < Rf_length(list); i++) {
    const char *current_name = CHAR(STRING_ELT(names, i));

    if (strcmp(key, current_name) == 0) {
      return VECTOR_ELT(list, i);  // Return matching element
    }
  }

  return R_NilValue;  // Return NULL if name not found
}


SEXP morloc_remote_call(SEXP midx, SEXP socket_path, SEXP cache_path, SEXP resources, SEXP arg_packets) { MAYFAIL
    // Protect all R inputs immediately
    PROTECT(socket_path);
    PROTECT(cache_path);
    PROTECT(resources);
    PROTECT(arg_packets = coerceVector(arg_packets, VECSXP));

    // Convert basic parameters
    int c_midx = INTEGER(midx)[0];
    const char* c_socket_path = CHAR(STRING_ELT(socket_path, 0));
    const char* c_cache_path = CHAR(STRING_ELT(cache_path, 0));

    // Extract resources with validation
    resources_t c_resources;
    SEXP mem = extract_element_by_name(resources, "memory");
    SEXP tim = extract_element_by_name(resources, "time");
    SEXP cpu = extract_element_by_name(resources, "cpus");
    SEXP gpu = extract_element_by_name(resources, "gpus");
    if (mem == R_NilValue || tim == R_NilValue || cpu == R_NilValue || gpu == R_NilValue) {
        UNPROTECT(4);
        MORLOC_ERROR("Missing required resource field (memory, time, cpus, or gpus)");
    }
    c_resources.memory = INTEGER(mem)[0];
    c_resources.time = INTEGER(tim)[0];
    c_resources.cpus = INTEGER(cpu)[0];
    c_resources.gpus = INTEGER(gpu)[0];

    // Process argument packets with type checking
    size_t nargs = LENGTH(arg_packets);
    const uint8_t** c_arg_packets = (const uint8_t**) R_alloc(nargs, sizeof(uint8_t*));

    for(size_t i = 0; i < nargs; i++) {
        SEXP raw_vec = VECTOR_ELT(arg_packets, i);
        if(TYPEOF(raw_vec) != RAWSXP) {
            UNPROTECT(4);
            MORLOC_ERROR("arg_packets must contain only raw vectors");
        }
        c_arg_packets[i] = (uint8_t*)RAW(raw_vec);
    }

    // Execute remote call
    uint8_t* result_packet = R_TRY_WITH(UNPROTECT(4),
        remote_call,
        c_midx,
        c_socket_path,
        c_cache_path,
        &c_resources,
        c_arg_packets,
        nargs
    );

    // Validate and copy result
    size_t packet_size = R_TRY_WITH({free(result_packet); UNPROTECT(4);}, morloc_packet_size, result_packet);
    if(!result_packet || packet_size == 0) {
        if(result_packet) free(result_packet);
        UNPROTECT(4);
        MORLOC_ERROR("Invalid result packet from remote call");
    }

    SEXP result_packet_r = PROTECT(allocVector(RAWSXP, packet_size));
    memcpy(RAW(result_packet_r), result_packet, packet_size);
    free(result_packet);

    // Cleanup and return
    UNPROTECT(5);  // socket_path, cache_path, resources, arg_packets, result_packet_r
    return result_packet_r;
}


// {{{ fork and fd-passing functions

SEXP morloc_socketpair(void) {
    int sv[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0) {
        error("socketpair failed: %s", strerror(errno));
    }
    SEXP result = PROTECT(allocVector(INTSXP, 2));
    INTEGER(result)[0] = sv[0];
    INTEGER(result)[1] = sv[1];
    UNPROTECT(1);
    return result;
}

SEXP morloc_fork(void) {
    pid_t pid = fork();
    if (pid < 0) {
        error("fork failed: %s", strerror(errno));
    }
    return ScalarInteger((int)pid);
}

// Immediately terminate the process without running any cleanup.
// Must be used by forked worker children instead of R's quit().
// R's quit() runs finalizers that try to free objects allocated by the
// parent process, which corrupts the heap on glibc >= 2.39.
SEXP morloc_exit(SEXP status_r) {
    int status = INTEGER(status_r)[0];
    _exit(status);
    return R_NilValue; // unreachable
}

SEXP morloc_send_fd(SEXP pipe_fd_r, SEXP client_fd_r) {
    int pipe_fd = INTEGER(pipe_fd_r)[0];
    int client_fd = INTEGER(client_fd_r)[0];

    struct msghdr msg = {0};
    struct iovec iov;
    char buf[1] = {0};
    char cmsgbuf[CMSG_SPACE(sizeof(int))];

    iov.iov_base = buf;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsgbuf;
    msg.msg_controllen = sizeof(cmsgbuf);

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(cmsg), &client_fd, sizeof(int));

    ssize_t n = sendmsg(pipe_fd, &msg, 0);
    if (n < 0) {
        error("sendmsg SCM_RIGHTS failed: %s", strerror(errno));
    }
    return R_NilValue;
}

SEXP morloc_recv_fd(SEXP pipe_fd_r) {
    int pipe_fd = INTEGER(pipe_fd_r)[0];

    struct msghdr msg = {0};
    struct iovec iov;
    char buf[1];
    char cmsgbuf[CMSG_SPACE(sizeof(int))];

    iov.iov_base = buf;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsgbuf;
    msg.msg_controllen = sizeof(cmsgbuf);

    ssize_t n = recvmsg(pipe_fd, &msg, 0);
    if (n <= 0) {
        return ScalarInteger(-1);
    }

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    if (cmsg == NULL || cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS) {
        return ScalarInteger(-1);
    }

    int fd;
    memcpy(&fd, CMSG_DATA(cmsg), sizeof(int));
    return ScalarInteger(fd);
}

SEXP morloc_kill(SEXP pid_r, SEXP sig_r) {
    pid_t pid = (pid_t)INTEGER(pid_r)[0];
    int sig = INTEGER(sig_r)[0];
    int ret = kill(pid, sig);
    return ScalarInteger(ret);
}

SEXP morloc_waitpid(SEXP pid_r) {
    pid_t pid = (pid_t)INTEGER(pid_r)[0];
    int status;
    pid_t result = waitpid(pid, &status, WNOHANG);
    return ScalarInteger((int)result);
}

SEXP morloc_waitpid_blocking(SEXP pid_r) {
    pid_t pid = (pid_t)INTEGER(pid_r)[0];
    int status;
    pid_t result = waitpid(pid, &status, 0);
    return ScalarInteger((int)result);
}

// }}} fork and fd-passing functions

// {{{ shared counter functions (for dynamic worker spawning)

static void shared_counter_finalizer(SEXP ptr) {
    int* p = (int*)R_ExternalPtrAddr(ptr);
    if (p != NULL) {
        munmap(p, sizeof(int));
        R_ClearExternalPtr(ptr);
    }
}

SEXP morloc_shared_counter_create(void) {
    int* p = (int*)mmap(NULL, sizeof(int),
                        PROT_READ | PROT_WRITE,
                        MAP_SHARED | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) {
        error("mmap failed for shared counter: %s", strerror(errno));
    }
    *p = 0;
    SEXP ptr = PROTECT(R_MakeExternalPtr(p, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ptr, shared_counter_finalizer, TRUE);
    UNPROTECT(1);
    return ptr;
}

SEXP morloc_shared_counter_inc(SEXP ptr_r) {
    int* p = (int*)R_ExternalPtrAddr(ptr_r);
    if (p == NULL) error("shared counter is NULL");
    int val = __atomic_add_fetch(p, 1, __ATOMIC_RELAXED);
    return ScalarInteger(val);
}

SEXP morloc_shared_counter_dec(SEXP ptr_r) {
    int* p = (int*)R_ExternalPtrAddr(ptr_r);
    if (p == NULL) error("shared counter is NULL");
    int val = __atomic_sub_fetch(p, 1, __ATOMIC_RELAXED);
    return ScalarInteger(val);
}

SEXP morloc_shared_counter_read(SEXP ptr_r) {
    int* p = (int*)R_ExternalPtrAddr(ptr_r);
    if (p == NULL) error("shared counter is NULL");
    int val = __atomic_load_n(p, __ATOMIC_RELAXED);
    return ScalarInteger(val);
}

SEXP morloc_pipe(void) {
    int fds[2];
    if (pipe(fds) != 0) {
        error("pipe failed: %s", strerror(errno));
    }
    SEXP result = PROTECT(allocVector(INTSXP, 2));
    INTEGER(result)[0] = fds[0];  /* read end */
    INTEGER(result)[1] = fds[1];  /* write end */
    UNPROTECT(1);
    return result;
}

SEXP morloc_write_byte(SEXP fd_r, SEXP byte_r) {
    int fd = INTEGER(fd_r)[0];
    unsigned char b = (unsigned char)RAW(byte_r)[0];
    ssize_t n = write(fd, &b, 1);
    return ScalarInteger((int)n);
}

SEXP morloc_close_fd(SEXP fd_r) {
    int fd = INTEGER(fd_r)[0];
    close(fd);
    return R_NilValue;
}

// }}} shared counter functions

// {{{ C-level worker loop

// Receive a file descriptor over a Unix domain socket (C-level helper).
static int recv_fd_c(int pipe_fd) {
    struct msghdr msg = {0};
    struct iovec iov;
    char buf[1];
    char cmsgbuf[CMSG_SPACE(sizeof(int))];

    iov.iov_base = buf;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsgbuf;
    msg.msg_controllen = sizeof(cmsgbuf);

    ssize_t n = recvmsg(pipe_fd, &msg, 0);
    if (n <= 0) return -1;

    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    if (!cmsg || cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS)
        return -1;

    int fd;
    memcpy(&fd, CMSG_DATA(cmsg), sizeof(int));
    return fd;
}

// Drain the morloc debug-trace (if --debug was compiled in and any
// frames were recorded). Returns a heap C string the caller must
// free, or NULL when no trace is present.
extern char* morloc_debug_drain_frames(void);

// Reset per-dispatch debug-trace state (called at the start of every
// run_job iteration so frames/dump counters don't leak between calls).
extern void morloc_debug_flush_dispatch(void);

// Record one frame to the per-thread debug-trace stack. Used by the
// codegen-emitted try/catch wraps in pool.R.
extern void morloc_debug_record_frame(
    uint32_t midx,
    const char* name,
    const char* srcloc,
    const char* lang,
    const uint8_t** packets,
    const char** schemas,
    size_t n);

// Send a fail packet to the client (best-effort, ignores send errors).
// Concatenates any recorded debug trace with the message so the nexus's
// summary.json carries both the foreign error and the morloc frames.
// Every manifold catch also appends its own "  at <name> [r] (mid=...,
// srcloc)" line to the exception message via string concat.
static void send_fail_to_client(int client_fd, const char* msg) {
    char* errmsg = NULL;
    char* trace = morloc_debug_drain_frames();
    char* full;
    if (trace == NULL) {
        full = (char*)msg;
    } else {
        size_t mlen = strlen(msg);
        size_t tlen = strlen(trace);
        full = (char*)malloc(mlen + tlen + 2);
        if (full) {
            memcpy(full, msg, mlen);
            full[mlen] = '\n';
            memcpy(full + mlen + 1, trace, tlen);
            full[mlen + 1 + tlen] = '\0';
        } else {
            full = (char*)msg;
        }
    }
    uint8_t* fail = make_fail_packet(full);
    send_packet_to_foreign_server(client_fd, fail, &errmsg);
    free(fail);
    if (trace != NULL) {
        free(trace);
        if (full != msg) free(full);
    }
}

// Dispatch a call to a manifold function. All packet handling is in C;
// only the manifold evaluation crosses into R via R_tryEval.
static void dispatch_manifold_c(int client_fd, const uint8_t* packet,
                                SEXP dispatch, const char* label) {
    char* errmsg = NULL;

    morloc_call_t* call = read_morloc_call_packet(packet, &errmsg);
    if (errmsg) {
        send_fail_to_client(client_fd, errmsg);
        return;
    }

    int midx = (int)call->midx;
    SEXP fn = (midx >= 1 && midx <= LENGTH(dispatch))
              ? VECTOR_ELT(dispatch, midx - 1) : R_NilValue;

    if (fn == R_NilValue) {
        char msg[128];
        snprintf(msg, sizeof(msg), "%s function not found: m%d", label, midx);
        send_fail_to_client(client_fd, msg);
        free_morloc_call(call);
        return;
    }

    // Build R pairlist of raw-vector arguments: fn(arg1, arg2, ...)
    int nprotect = 0;
    SEXP pairlist = R_NilValue;
    for (int i = (int)call->nargs - 1; i >= 0; i--) {
        size_t arg_size = morloc_packet_size(call->args[i], &errmsg);
        if (errmsg) {
            UNPROTECT(nprotect);
            send_fail_to_client(client_fd, errmsg);
            free_morloc_call(call);
            return;
        }
        SEXP r_arg = PROTECT(allocVector(RAWSXP, arg_size));
        nprotect++;
        memcpy(RAW(r_arg), call->args[i], arg_size);
        pairlist = PROTECT(Rf_cons(r_arg, pairlist));
        nprotect++;
    }
    free_morloc_call(call);

    SEXP r_call = PROTECT(Rf_lcons(fn, pairlist));
    nprotect++;

    // Single crossing into R: evaluate the manifold
    int eval_err = 0;
    SEXP result = R_tryEvalSilent(r_call, R_GlobalEnv, &eval_err);

    if (eval_err || result == R_NilValue || TYPEOF(result) != RAWSXP) {
        UNPROTECT(nprotect);
        send_fail_to_client(client_fd,
            eval_err ? R_curErrorBuf() : "manifold returned non-raw result");
        return;
    }

    PROTECT(result);
    nprotect++;

    send_packet_to_foreign_server(client_fd, RAW(result), &errmsg);
    UNPROTECT(nprotect);
}

// Process one client job entirely in C. Only crosses into R for
// the actual manifold evaluation.
static void run_job_c(int client_fd, SEXP dispatch, SEXP remote_dispatch) {
    char* errmsg = NULL;

    // Release any SHM that the previous request's morloc_put_value handed
    // off via PACKET_SOURCE_RPTR. The block stays alive until now so the
    // calling daemon can dereference its relptr; once we're starting a new
    // request, it is safe to reclaim. Without this every RPTR result would
    // leak per call and grow /dev/shm/morloc-* monotonically.
    shm_tracker_flush();
    morloc_debug_flush_dispatch();

    uint8_t* packet = stream_from_client(client_fd, &errmsg);
    if (errmsg) {
        send_fail_to_client(client_fd, errmsg);
        free(errmsg);
        close_socket(client_fd);
        return;
    }

    bool is_local = packet_is_local_call(packet, &errmsg);
    if (!errmsg && is_local) {
        dispatch_manifold_c(client_fd, packet, dispatch, "Local");
    } else if (!errmsg) {
        bool is_remote = packet_is_remote_call(packet, &errmsg);
        if (!errmsg && is_remote) {
            dispatch_manifold_c(client_fd, packet, remote_dispatch, "Remote");
        } else if (!errmsg) {
            bool is_ping_pkt = packet_is_ping(packet, &errmsg);
            if (!errmsg && is_ping_pkt) {
                uint8_t* pong = return_ping(packet, &errmsg);
                if (!errmsg) {
                    send_packet_to_foreign_server(client_fd, pong, &errmsg);
                    free(pong);
                }
            } else if (!errmsg) {
                send_fail_to_client(client_fd, "Unexpected packet type");
            }
        }
    }

    if (errmsg) {
        send_fail_to_client(client_fd, errmsg);
    }

    free(packet);
    close_socket(client_fd);
}

// Tight C worker loop. Receives fds from the job queue and processes them,
// crossing into R only for manifold evaluation.
SEXP morloc_worker_loop_c(SEXP pipe_fd_r, SEXP dispatch_r, SEXP remote_dispatch_r) {
    int pipe_fd = INTEGER(pipe_fd_r)[0];
    PROTECT(dispatch_r);
    PROTECT(remote_dispatch_r);

    while (!r_shutting_down) {
        int client_fd = recv_fd_c(pipe_fd);
        if (client_fd < 0) break;
        run_job_c(client_fd, dispatch_r, remote_dispatch_r);
        fflush(stdout);
    }

    UNPROTECT(2);
    return R_NilValue;
}

// }}} C-level worker loop

// ── Cache bridge to libmorloc.so ───────────────────────────────────────────

// Compute a cache key. Inputs:
//   midx_r:     integer manifold id
//   packets_r:  list of raw vectors (one packet per arg)
//   schemas_r:  character vector of schema strings (one per arg, same length)
// Returns a length-1 numeric (the 64-bit key cast to double; bit-exact up to
// 2^53, fine for our xxh64 keys round-tripping back into other cache calls).
SEXP morloc_cache_key_compute_r(SEXP midx_r, SEXP packets_r, SEXP schemas_r) {
    if (TYPEOF(midx_r) != INTSXP || LENGTH(midx_r) != 1) {
        MORLOC_ERROR("cache_key_compute: midx must be a single integer");
    }
    if (TYPEOF(packets_r) != VECSXP) {
        MORLOC_ERROR("cache_key_compute: packets must be a list of raw vectors");
    }
    if (TYPEOF(schemas_r) != STRSXP) {
        MORLOC_ERROR("cache_key_compute: schemas must be a character vector");
    }
    R_xlen_t n_args = XLENGTH(packets_r);
    if (XLENGTH(schemas_r) != n_args) {
        MORLOC_ERROR("cache_key_compute: packets and schemas must have equal length");
    }

    const uint8_t** packet_ptrs = (const uint8_t**)R_alloc(n_args, sizeof(uint8_t*));
    const char** schema_ptrs = (const char**)R_alloc(n_args, sizeof(char*));
    for (R_xlen_t i = 0; i < n_args; i++) {
        SEXP p = VECTOR_ELT(packets_r, i);
        if (TYPEOF(p) != RAWSXP) {
            MORLOC_ERROR("cache_key_compute: packet %lld must be a raw vector",
                (long long)i);
        }
        packet_ptrs[i] = (const uint8_t*)RAW(p);
        SEXP s = STRING_ELT(schemas_r, i);
        if (s == NA_STRING) {
            MORLOC_ERROR("cache_key_compute: schema %lld is NA", (long long)i);
        }
        schema_ptrs[i] = CHAR(s);
    }

    char* errmsg = NULL;
    uint64_t key = morloc_cache_key_compute(
        (uint32_t)INTEGER(midx_r)[0],
        packet_ptrs,
        schema_ptrs,
        (size_t)n_args,
        &errmsg
    );
    if (errmsg) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "%s", errmsg);
        free(errmsg);
        MORLOC_ERROR("cache_key_compute failed: %s", buf);
    }
    return ScalarReal((double)key);
}

// debug_record_frame(midx, packets_list, schemas_list)
// packets_list: list of raw vectors (serialized packets, one per arg).
// schemas_list: character vector (schema strings).
// Appends one frame to the per-thread debug-trace stack.
SEXP morloc_debug_flush_dispatch_r(void) {
    morloc_debug_flush_dispatch();
    return R_NilValue;
}

SEXP morloc_debug_record_frame_r(SEXP midx_r, SEXP name_r, SEXP srcloc_r,
                                 SEXP lang_r, SEXP packets_r, SEXP schemas_r) {
    if (TYPEOF(midx_r) != INTSXP || LENGTH(midx_r) != 1) {
        MORLOC_ERROR("debug_record_frame: midx must be a single integer");
    }
    if (TYPEOF(name_r) != STRSXP || LENGTH(name_r) != 1) {
        MORLOC_ERROR("debug_record_frame: name must be a single string");
    }
    if (TYPEOF(srcloc_r) != STRSXP || LENGTH(srcloc_r) != 1) {
        MORLOC_ERROR("debug_record_frame: srcloc must be a single string");
    }
    if (TYPEOF(lang_r) != STRSXP || LENGTH(lang_r) != 1) {
        MORLOC_ERROR("debug_record_frame: lang must be a single string");
    }
    if (TYPEOF(packets_r) != VECSXP) {
        MORLOC_ERROR("debug_record_frame: packets must be a list of raw vectors");
    }
    if (TYPEOF(schemas_r) != STRSXP) {
        MORLOC_ERROR("debug_record_frame: schemas must be a character vector");
    }
    R_xlen_t n_args = XLENGTH(packets_r);
    if (XLENGTH(schemas_r) != n_args) {
        MORLOC_ERROR("debug_record_frame: packets and schemas length mismatch");
    }
    // "" == "no info attached"; the Rust runtime treats "" and NULL the
    // same. NA_STRING falls back to "" here so downstream sees empty.
    SEXP name_elt = STRING_ELT(name_r, 0);
    SEXP srcloc_elt = STRING_ELT(srcloc_r, 0);
    SEXP lang_elt = STRING_ELT(lang_r, 0);
    const char* name_c = (name_elt == NA_STRING) ? "" : CHAR(name_elt);
    const char* srcloc_c = (srcloc_elt == NA_STRING) ? "" : CHAR(srcloc_elt);
    const char* lang_c = (lang_elt == NA_STRING) ? "" : CHAR(lang_elt);
    if (n_args == 0) {
        morloc_debug_record_frame((uint32_t)INTEGER(midx_r)[0], name_c, srcloc_c,
                                  lang_c, NULL, NULL, 0);
        return R_NilValue;
    }
    const uint8_t** packet_ptrs = (const uint8_t**)R_alloc(n_args, sizeof(uint8_t*));
    const char** schema_ptrs = (const char**)R_alloc(n_args, sizeof(char*));
    for (R_xlen_t i = 0; i < n_args; i++) {
        SEXP p = VECTOR_ELT(packets_r, i);
        if (TYPEOF(p) != RAWSXP) {
            MORLOC_ERROR("debug_record_frame: packet %lld must be raw", (long long)i);
        }
        packet_ptrs[i] = (const uint8_t*)RAW(p);
        SEXP s = STRING_ELT(schemas_r, i);
        if (s == NA_STRING) {
            MORLOC_ERROR("debug_record_frame: schema %lld is NA", (long long)i);
        }
        schema_ptrs[i] = CHAR(s);
    }
    morloc_debug_record_frame((uint32_t)INTEGER(midx_r)[0], name_c, srcloc_c,
        lang_c, packet_ptrs, schema_ptrs, (size_t)n_args);
    return R_NilValue;
}

SEXP morloc_cache_lookup_r(SEXP key_r, SEXP label_r) {
    uint64_t key = (uint64_t)asReal(key_r);
    if (TYPEOF(label_r) != STRSXP || LENGTH(label_r) != 1) {
        MORLOC_ERROR("cache_lookup: label must be a single string");
    }
    const char* label = CHAR(STRING_ELT(label_r, 0));
    size_t size = 0;
    char* errmsg = NULL;
    uint8_t* data = morloc_cache_lookup(key, label, &size, &errmsg);
    if (errmsg) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "%s", errmsg);
        free(errmsg);
        MORLOC_ERROR("cache_lookup failed: %s", buf);
    }
    if (!data) {
        return R_NilValue;
    }
    SEXP result = PROTECT(allocVector(RAWSXP, size));
    memcpy(RAW(result), data, size);
    free(data);
    UNPROTECT(1);
    return result;
}

SEXP morloc_cache_store_r(SEXP key_r, SEXP label_r, SEXP packet_r, SEXP schema_r) {
    uint64_t key = (uint64_t)asReal(key_r);
    if (TYPEOF(label_r) != STRSXP || LENGTH(label_r) != 1) {
        MORLOC_ERROR("cache_store: label must be a single string");
    }
    if (TYPEOF(packet_r) != RAWSXP) {
        MORLOC_ERROR("cache_store: packet must be a raw vector");
    }
    if (TYPEOF(schema_r) != STRSXP || LENGTH(schema_r) != 1) {
        MORLOC_ERROR("cache_store: schema must be a single string");
    }
    const char* label = CHAR(STRING_ELT(label_r, 0));
    const char* schema = CHAR(STRING_ELT(schema_r, 0));
    char* errmsg = NULL;
    bool ok = morloc_cache_store(
        key, label,
        (const uint8_t*)RAW(packet_r),
        (size_t)XLENGTH(packet_r),
        schema,
        &errmsg
    );
    if (!ok) {
        if (errmsg) {
            char buf[1024];
            snprintf(buf, sizeof(buf), "%s", errmsg);
            free(errmsg);
            MORLOC_ERROR("cache_store failed: %s", buf);
        }
        MORLOC_ERROR("cache_store failed");
    }
    return R_NilValue;
}

SEXP morloc_cache_record_hit_r(void) {
    morloc_cache_record_hit();
    return R_NilValue;
}

SEXP morloc_cache_record_miss_r(void) {
    morloc_cache_record_miss();
    return R_NilValue;
}

SEXP morloc_cache_record_store_r(void) {
    morloc_cache_record_store();
    return R_NilValue;
}

// }}} exported functions


// R extracts the package name from the .so filename by stripping the "lib"
// prefix and ".so" suffix and looks for R_init_<that name>. Our shared
// library is installed as "librmorloc.so", so the expected initializer is
// "R_init_librmorloc" -- not "R_init_rmorloc". We expose both names as
// aliases for the same body so the registration runs regardless of which
// stripping convention a given R build follows.
static void __attribute__((unused)) _r_init_impl(DllInfo *info);
void R_init_librmorloc(DllInfo *info) { _r_init_impl(info); }
void R_init_rmorloc(DllInfo *info) { _r_init_impl(info); }

static void _r_init_impl(DllInfo *info) {
    R_CallMethodDef callMethods[] = {
        {"morloc_start_daemon", (DL_FUNC) &morloc_start_daemon, 4},
        {"morloc_wait_for_client", (DL_FUNC) &morloc_wait_for_client, 1},
        {"morloc_read_morloc_call_packet", (DL_FUNC) &morloc_read_morloc_call_packet, 1},
        {"morloc_send_packet_to_foreign_server", (DL_FUNC) &morloc_send_packet_to_foreign_server, 2},
        {"morloc_stream_from_client", (DL_FUNC) &morloc_stream_from_client, 1},
        {"morloc_close_socket", (DL_FUNC) &morloc_close_socket, 1},
        {"morloc_foreign_call", (DL_FUNC) &morloc_foreign_call, 3},
        {"morloc_get_value", (DL_FUNC) &morloc_get_value, 2},
        {"morloc_put_value", (DL_FUNC) &morloc_put_value, 2},
        {"morloc_mlc_show", (DL_FUNC) &morloc_mlc_show, 2},
        {"r_morloc_log_next_id", (DL_FUNC) &morloc_log_next_id_r, 0},
        {"r_morloc_log_emit", (DL_FUNC) &morloc_log_emit_r, 4},
        {"morloc_is_ping", (DL_FUNC) &morloc_is_ping, 1},
        {"morloc_is_local_call", (DL_FUNC) &morloc_is_local_call, 1},
        {"morloc_is_remote_call", (DL_FUNC) &morloc_is_remote_call, 1},
        {"morloc_remote_call", (DL_FUNC) &morloc_remote_call, 5},
        {"morloc_pong", (DL_FUNC) &morloc_pong, 1},
        {"morloc_make_fail_packet", (DL_FUNC) &morloc_make_fail_packet, 1},
        {"morloc_release_packet_shm", (DL_FUNC) &morloc_release_packet_shm, 1},
        {"morloc_shinit", (DL_FUNC) &morloc_shinit, 3},
        {"morloc_socketpair", (DL_FUNC) &morloc_socketpair, 0},
        {"morloc_fork", (DL_FUNC) &morloc_fork, 0},
        {"morloc_exit", (DL_FUNC) &morloc_exit, 1},
        {"morloc_send_fd", (DL_FUNC) &morloc_send_fd, 2},
        {"morloc_recv_fd", (DL_FUNC) &morloc_recv_fd, 1},
        {"morloc_kill", (DL_FUNC) &morloc_kill, 2},
        {"morloc_waitpid", (DL_FUNC) &morloc_waitpid, 1},
        {"morloc_waitpid_blocking", (DL_FUNC) &morloc_waitpid_blocking, 1},
        {"morloc_install_sigterm_handler", (DL_FUNC) &morloc_install_sigterm_handler, 0},
        {"morloc_set_line_buffered", (DL_FUNC) &morloc_set_line_buffered, 0},
        {"morloc_is_shutting_down", (DL_FUNC) &morloc_is_shutting_down, 0},
        {"morloc_detach_daemon", (DL_FUNC) &morloc_detach_daemon, 1},
        {"morloc_shared_counter_create", (DL_FUNC) &morloc_shared_counter_create, 0},
        {"morloc_shared_counter_inc", (DL_FUNC) &morloc_shared_counter_inc, 1},
        {"morloc_shared_counter_dec", (DL_FUNC) &morloc_shared_counter_dec, 1},
        {"morloc_shared_counter_read", (DL_FUNC) &morloc_shared_counter_read, 1},
        {"morloc_pipe", (DL_FUNC) &morloc_pipe, 0},
        {"morloc_write_byte", (DL_FUNC) &morloc_write_byte, 2},
        {"morloc_close_fd", (DL_FUNC) &morloc_close_fd, 1},
        {"morloc_worker_loop_c", (DL_FUNC) &morloc_worker_loop_c, 3},
        {"r_morloc_cache_key_compute", (DL_FUNC) &morloc_cache_key_compute_r, 3},
        {"r_morloc_debug_record_frame", (DL_FUNC) &morloc_debug_record_frame_r, 6},
        {"r_morloc_debug_flush_dispatch", (DL_FUNC) &morloc_debug_flush_dispatch_r, 0},
        {"r_morloc_cache_lookup", (DL_FUNC) &morloc_cache_lookup_r, 2},
        {"r_morloc_cache_store", (DL_FUNC) &morloc_cache_store_r, 4},
        {"r_morloc_cache_record_hit", (DL_FUNC) &morloc_cache_record_hit_r, 0},
        {"r_morloc_cache_record_miss", (DL_FUNC) &morloc_cache_record_miss_r, 0},
        {"r_morloc_cache_record_store", (DL_FUNC) &morloc_cache_record_store_r, 0},
        {"morloc_mlc_open", (DL_FUNC) &morloc_mlc_open, 2},
        {"morloc_mlc_close", (DL_FUNC) &morloc_mlc_close, 1},
        {"morloc_mlc_fschema", (DL_FUNC) &morloc_mlc_fschema, 1},
        {"morloc_mlc_ifile_walk", (DL_FUNC) &morloc_mlc_ifile_walk, 4},
        {"morloc_mlc_ifile_length", (DL_FUNC) &morloc_mlc_ifile_length, 1},
        {"morloc_mlc_next", (DL_FUNC) &morloc_mlc_next, 2},
        {"morloc_mlc_stream", (DL_FUNC) &morloc_mlc_stream, 1},
        {"morloc_mlc_open_ostream", (DL_FUNC) &morloc_mlc_open_ostream, 2},
        {"morloc_mlc_open_stdin",   (DL_FUNC) &morloc_mlc_open_stdin,   1},
        {"morloc_mlc_open_stdout",  (DL_FUNC) &morloc_mlc_open_stdout,  1},
        {"morloc_mlc_open_stderr",  (DL_FUNC) &morloc_mlc_open_stderr,  1},
        {"morloc_mlc_write", (DL_FUNC) &morloc_mlc_write, 4},
        {"morloc_mlc_append", (DL_FUNC) &morloc_mlc_append, 2},
        {"morloc_mlc_concat", (DL_FUNC) &morloc_mlc_concat, 2},
        {"morloc_mlc_flush", (DL_FUNC) &morloc_mlc_flush, 1},
        {"morloc_mlc_save_voidstar", (DL_FUNC) &morloc_mlc_save_voidstar, 4},
        {"morloc_mlc_save", (DL_FUNC) &morloc_mlc_save, 4},
        {"morloc_mlc_save_json", (DL_FUNC) &morloc_mlc_save_json, 4},
        {"morloc_mlc_hash", (DL_FUNC) &morloc_mlc_hash, 2},
        {"morloc_mlc_load", (DL_FUNC) &morloc_mlc_load, 2},
        {"morloc_mlc_read", (DL_FUNC) &morloc_mlc_read, 2},
        {NULL, NULL, 0}
    };

    R_registerRoutines(info, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(info, FALSE);
}
