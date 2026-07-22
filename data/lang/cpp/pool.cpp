#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <algorithm>
#include <iterator>
#include <climits>
#include <stdexcept>
#include <fstream>
#include <system_error>
#include <unordered_map>
#include <sys/stat.h>
#include <sys/mman.h>
#include <csignal>
#ifdef __linux__
#include <sys/prctl.h>
#endif

// needed for foreign interface
#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <cstring>
#include <unistd.h>

#include <limits>
#include <utility>
#include <chrono>

char* g_tmpdir;

uint8_t* foreign_call(const char* socket_filename, size_t mid, ...) __attribute__((sentinel));

// AUTO include statements start
// <<<BREAK>>>
// AUTO include statements end

// Proper linking of cppmorloc requires it be included AFTER the custom modules
#include "mlc_arrow.hpp"
#include "cppmorloc.hpp"

// Defines mlc::Unit, which the generator emits for do-blocks whose final
// expression is void-returning (e.g. @save/@savej/@savem). Needs to follow
// the user-source includes so foreign code can use mlc::Unit too.
#include "mlccpptypes/prelude.hpp"

// Forward-declared here so PROPAGATE_ERROR can reference it before the
// class body is fully defined further down. MorlocException is the
// user-catchable class (@catch intercepts). Compiler / invariant bugs
// go through _mlc_internal_abort (below), which bypasses @catch.
class MorlocException;

#define PROPAGATE_ERROR(errmsg) \
    if(errmsg != NULL) { \
      char errmsg_buffer[MAX_ERRMSG_SIZE] = { 0 }; \
      snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error C++ pool (%s:%d in %s):\n%s" , __FILE__, __LINE__, __func__, errmsg); \
      free(errmsg); \
      throw MorlocException(errmsg_buffer); \
    }

// Terminal helper for morloc-compiler / runtime invariant violations
// (unreachable branches, contract violations from libmorloc, arity
// mismatches emitted by codegen). Prints a diagnostic to stderr, then
// aborts the pool process. The nexus sees the pool crash and treats
// it as a fatal error -- @catch never gets a chance to intercept.
// Use this only for genuine bugs: any error attributable to user data,
// paths, ascriptions, or foreign-function behavior must go through
// PROPAGATE_ERROR / MorlocException instead so @catch works.
[[noreturn]] static inline void _mlc_internal_abort(
    const char* file, int line, const char* func, const char* msg
) {
    std::fprintf(
        stderr,
        "morloc internal error (C++ pool, %s:%d in %s):\n  %s\n",
        file, line, func, msg
    );
    std::abort();
}

#define MLC_INTERNAL_ABORT(msg) _mlc_internal_abort(__FILE__, __LINE__, __func__, (msg))

#define PROPAGATE_FAIL_PACKET(errmsg) \
    if(errmsg != NULL){ \
        uint8_t* fail_packet_ = make_fail_packet(errmsg); \
        free(errmsg); \
        return fail_packet_; \
    }


// AUTO serialization statements start
// <<<BREAK>>>
// AUTO serialization statements end



std::string interweave_strings(const std::vector<std::string>& first, const std::vector<std::string>& second)
{
    // Validate sizes - errors here indicate a bug in the morloc compiler
    if (first.size() != second.size() + 1) {
        MLC_INTERNAL_ABORT(
            "interweave_strings: first list must have exactly 1 more element than second list"
        );
    }

    // Pre-calculate total size to avoid reallocations
    size_t total_size = 0;
    for (const auto& s : first) total_size += s.size();
    for (const auto& s : second) total_size += s.size();

    std::string result;
    result.reserve(total_size);

    // Interweave the strings
    for (size_t i = 0; i < second.size(); ++i) {
        result += first[i];
        result += second[i];
    }
    result += first.back();  // Append the final element from first list

    return result;
}

// Thread-local list of SHM pointers allocated by _put_value.
// Freed after foreign_call returns (args consumed) or at next dispatch start
// (result consumed by caller in the synchronous call that returned it).
struct ShmEntry { absptr_t ptr; Schema* schema; };
thread_local std::vector<ShmEntry> _shm_tracker;

static void _shm_tracker_flush() {
    for (auto& e : _shm_tracker) {
        char* err = NULL;
        // shfree decrements the refcount and zeros the block on final
        // ref-drop, so a separate metadata-zeroing pass is unnecessary.
        shfree(e.ptr, &err);
        if (err) { free(err); }
    }
    _shm_tracker.clear();
}

// Drop one tracker entry matching ptr (swap-with-last), shfree the
// block, and free its schema. Used by _release_packet_shm to free
// a _put_value-tracked packet's SHM as soon as its codegen-determined
// scope ends, rather than waiting for the next dispatch flush.
static bool _shm_tracker_release_one(absptr_t ptr) {
    for (size_t i = 0; i < _shm_tracker.size(); i++) {
        if (_shm_tracker[i].ptr == ptr) {
            Schema* schema = _shm_tracker[i].schema;
            _shm_tracker[i] = _shm_tracker.back();
            _shm_tracker.pop_back();
            char* err = NULL;
            shfree(ptr, &err);
            if (err) { free(err); }
            if (schema) { free_schema(schema); }
            return true;
        }
    }
    return false;
}

// Release the SHM ref owned by a _put_value-produced packet. The codegen
// inserts this call at the end of a serialize let's scope so the tracker
// entry is dropped as soon as the packet is no longer needed. No-op for
// inline (non-RPTR) packets, so callers can invoke unconditionally.
static void _release_packet_shm(const uint8_t* packet) {
    if (packet == nullptr) return;
    const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
    if (hdr->command.data.source != PACKET_SOURCE_RPTR) return;
    size_t relptr = *(size_t*)(packet
        + sizeof(morloc_packet_header_t) + hdr->offset);
    char* resolve_err = NULL;
    void* voidstar = rel2abs(relptr, &resolve_err);
    if (resolve_err) { free(resolve_err); resolve_err = NULL; }
    if (voidstar) {
        _shm_tracker_release_one((absptr_t)voidstar);
    }
}

// Transforms a serialized value into a message ready for the socket
template <typename T>
uint8_t* _put_value(const T& value, Schema* schema) {
    // Push the top-level schema's recursion declaration (if any) onto
    // the env stack so Recur back-references inside the walk can
    // resolve to their named target. RAII pops on every return path.
    RecurEnvScope _recur_top(schema);

    if constexpr (std::is_same_v<T, mlc::ArrowTable>) {
        // Arrow export: move table data into SHM, build packet.
        // const_cast is safe here: the value is always a temporary from
        // a manifold call, never a truly const object.
        mlc::ArrowTable& tbl = const_cast<mlc::ArrowTable&>(value);
        relptr_t relptr = tbl.move_to_shm();

        uint8_t* packet = make_arrow_data_packet(relptr, schema);
        if (!packet) { MLC_INTERNAL_ABORT("failed to create arrow data packet"); }

        char* err = nullptr;
        void* shm_ptr = rel2abs(relptr, &err);
        if (err) { free(err); }
        if (shm_ptr) { _shm_tracker.push_back({(absptr_t)shm_ptr, nullptr}); }
        return packet;
    } else {
        // Arrow dispatch: schema marker `T` (MORLOC_TABLE) routes through
        // mlc::ArrowTable. The legacy `<arrow>` hint has been retired.
        if (schema->type == MORLOC_TABLE) {
            MLC_INTERNAL_ABORT("table schema but C++ type is not mlc::ArrowTable");
        }

        void* voidstar = nullptr;
        try {
            voidstar = to_voidstar(schema, value);
            relptr_t relptr = abs2rel_cpp(voidstar);

            char* errmsg = nullptr;
            uint8_t* packet = make_data_packet_auto(voidstar, relptr, schema, &errmsg);
            if (errmsg) {
                shfree_cpp(voidstar);
                PROPAGATE_ERROR(errmsg);
            }

            const morloc_packet_header_t* hdr = (const morloc_packet_header_t*)packet;
            if (hdr->command.data.source == PACKET_SOURCE_RPTR) {
                // SHM referenced by packet -- track for deferred cleanup
                _shm_tracker.push_back({(absptr_t)voidstar, schema});
            } else {
                // Data inlined in packet -- free SHM immediately. shfree
                // zeros the block on final ref-drop.
                char* free_err = NULL;
                shfree((absptr_t)voidstar, &free_err);
                if (free_err) { free(free_err); }
            }
            return packet;
        } catch (...) {
            if (voidstar) shfree_cpp(voidstar);
            throw;
        }
    }
}


// Use a key to retrieve a value
template <typename T>
T _get_value(const uint8_t* packet, Schema* schema){
    // Push the top-level schema's recursion declaration (if any) so
    // Recur back-references inside the walk can resolve. RAII pops on
    // every return path including exceptions.
    RecurEnvScope _recur_top(schema);
    const morloc_packet_header_t* header = (const morloc_packet_header_t*)packet;
    uint8_t source = header->command.data.source;
    uint8_t format = header->command.data.format;

    if constexpr (std::is_same_v<T, mlc::ArrowTable>) {
        // Arrow import: packet -> arrow_from_shm -> ArrowTable
        char* errmsg = nullptr;
        uint8_t* raw = get_morloc_data_packet_value(packet, schema, &errmsg);
        if (errmsg) { PROPAGATE_ERROR(errmsg); }

        const arrow_shm_header_t* hdr = (const arrow_shm_header_t*)raw;
        struct ArrowSchema as;
        struct ArrowArray aa;
        char* aerr = nullptr;
        arrow_from_shm(hdr, &as, &aa, &aerr);
        if (aerr) { PROPAGATE_ERROR(aerr); }

        char* ierr = nullptr;
        shincref((absptr_t)raw, &ierr);
        if (ierr) { free(ierr); }
        _shm_tracker.push_back({(absptr_t)raw, nullptr});

        return mlc::ArrowTable(std::move(as), std::move(aa));
    } else {
        if (format == PACKET_FORMAT_ARROW) {
            MLC_INTERNAL_ABORT("arrow data but C++ type is not mlc::ArrowTable");
        }

        // Stream ingest: FILE+DATA pointing at a stream-packet file. Nexus
        // emits this shape for `[a]` receivers; peek here to distinguish
        // stream from ordinary data-indirection. Each chunk's SHM is
        // shfree'd before the next @next, so peak SHM stays at one
        // sub-packet. Non-vector T instantiations throw at runtime as
        // defence-in-depth (nexus rejects such receivers upstream).
        if (source == PACKET_SOURCE_FILE && format == PACKET_FORMAT_DATA) {
            size_t payload_len = header->length;
            char path[PATH_MAX];
            if (payload_len >= sizeof(path)) {
                throw MorlocException("stream ingest: path exceeds PATH_MAX");
            }
            std::memcpy(path,
                        packet + sizeof(morloc_packet_header_t) + header->offset,
                        payload_len);
            path[payload_len] = '\0';
            if (file_is_stream_packet(path)) {
                if constexpr (is_std_vector<T>::value) {
                    char* open_err = nullptr;
                    int64_t handle = mlc_open(path, MLC_KIND_ISTREAM, &open_err);
                    if (open_err) { PROPAGATE_ERROR(open_err); }
                    // Preallocate via the stream's element_count (from
                    // StreamDiag) so vector::insert never reallocates.
                    char* len_err = nullptr;
                    int64_t total = mlc_ifile_length(handle, &len_err);
                    T result;
                    if (!len_err && total > 0) {
                        result.reserve((size_t)total);
                    }
                    if (len_err) free(len_err);
                    try {
                        while (true) {
                            char* nerr = nullptr;
                            void* chunk = mlc_next(handle, &nerr);
                            if (nerr) {
                                char* cerr = nullptr;
                                mlc_close(handle, &cerr);
                                if (cerr) free(cerr);
                                PROPAGATE_ERROR(nerr);
                            }
                            if (chunk == nullptr) break;
                            Array* arr = (Array*)chunk;
                            if (arr->size == 0) {
                                char* ferr = nullptr;
                                shfree((absptr_t)chunk, &ferr);
                                if (ferr) free(ferr);
                                break;
                            }
                            T* dummy = nullptr;
                            T chunk_vec = from_voidstar(schema, chunk, dummy);
                            char* ferr = nullptr;
                            shfree((absptr_t)chunk, &ferr);
                            if (ferr) free(ferr);
                            result.insert(result.end(),
                                          std::make_move_iterator(chunk_vec.begin()),
                                          std::make_move_iterator(chunk_vec.end()));
                        }
                    } catch (...) {
                        char* cerr = nullptr;
                        mlc_close(handle, &cerr);
                        if (cerr) free(cerr);
                        throw;
                    }
                    char* cerr = nullptr;
                    mlc_close(handle, &cerr);
                    if (cerr) free(cerr);
                    return result;
                } else {
                    MLC_INTERNAL_ABORT(
                        "stream indirection packet received for a "
                        "non-list receiver type; nexus should have rejected this upstream"
                    );
                }
            }
        }

        // Fast path: inline voidstar -- read directly from packet, no SHM needed
        if (source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_VOIDSTAR) {
            const uint8_t* payload = packet + sizeof(morloc_packet_header_t) + header->offset;
            T* dummy = nullptr;
            return from_voidstar(schema, (const void*)payload, dummy, (const void*)payload);
        }

        // SHM paths (RPTR or MESG+MSGPACK): existing logic
        bool is_rptr = (source == PACKET_SOURCE_RPTR);

        char* errmsg = NULL;
        uint8_t* voidstar = get_morloc_data_packet_value(packet, schema, &errmsg);
        if(errmsg != NULL) {
            PROPAGATE_ERROR(errmsg)
        }

        // For RPTR data, increment refcount so the owner's tracker flush
        // won't destroy data we may still need (e.g. forwarded packets).
        if (is_rptr) {
            char* incref_err = NULL;
            shincref((absptr_t)voidstar, &incref_err);
            if (incref_err) { free(incref_err); }
            _shm_tracker.push_back({(absptr_t)voidstar, schema});
        }

        T* dummy = nullptr;
        return from_voidstar(schema, (void*)voidstar, dummy);
    }
}


// Hash a value, returning a 16-char hex string
template <typename T>
std::string _mlc_hash(const T& value, Schema* schema) {
    void* voidstar = to_voidstar(schema, value);
    char* errmsg = NULL;
    char* hex = mlc_hash(voidstar, schema, &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
    std::string result(hex);
    free(hex);
    return result;
}

// Save a value to file in msgpack format. The `level` arg is accepted
// for ABI uniformity with mlc_save_voidstar and forwarded; the runtime
// ignores it for non-packet formats.
template <typename T>
void _mlc_save(const T& value, Schema* schema, int64_t level, const std::string& path) {
    void* voidstar = to_voidstar(schema, value);
    char* errmsg = NULL;
    mlc_save(voidstar, schema, (uint8_t)level, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Save a value to file in flat voidstar binary format. `level` is the
// zstd preset (0 = uncompressed, 1-9 = increasing ratio); the runtime
// stamps PACKET_COMPRESSION_ZSTD into the packet header when level > 0.
template <typename T>
void _mlc_save_voidstar(const T& value, Schema* schema, int64_t level, const std::string& path) {
    void* voidstar = to_voidstar(schema, value);
    char* errmsg = NULL;
    mlc_save_voidstar(voidstar, schema, (uint8_t)level, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Save a value to file in JSON format. `level` accepted for ABI uniformity.
template <typename T>
void _mlc_save_json(const T& value, Schema* schema, int64_t level, const std::string& path) {
    void* voidstar = to_voidstar(schema, value);
    char* errmsg = NULL;
    mlc_save_json(voidstar, schema, (uint8_t)level, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Serialize a value to a JSON string
template <typename T>
std::string _mlc_show(const T& value, Schema* schema) {
    void* voidstar = to_voidstar(schema, value);
    char* errmsg = NULL;
    char* json = mlc_show(voidstar, schema, &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
    std::string result(json);
    free(json);
    return result;
}

// MorlocException carries a @throw / catchable-intrinsic-failure
// message across the pool call stack. Defined here (before _mlc_read /
// _mlc_load) so those shims can throw it directly on failure. Also
// used by _mlc_throw (below) and caught by _mlc_catch.
class MorlocException : public std::runtime_error {
public:
    using std::runtime_error::runtime_error;
};

// Deserialize a JSON string to a typed value.
// @read :: Str -> <Err> a. Parse failure throws MorlocException so
// _mlc_catch can intercept.
template <typename T>
T _mlc_read(Schema* schema, const std::string& json_str) {
    char* errmsg = NULL;
    void* voidstar = mlc_read(json_str.c_str(), schema, &errmsg);
    if (errmsg != NULL) {
        std::string msg(errmsg);
        free(errmsg);
        throw MorlocException(std::string("@read: ") + msg);
    }
    if (voidstar == NULL) {
        throw MorlocException("@read: parse failed on '" + json_str + "'");
    }
    T* dummy = nullptr;
    T result = from_voidstar(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
}

// Load a value from file, auto-detecting format.
// @load :: Str -> <IO, Err> a. Missing file / decode failure throws
// MorlocException so _mlc_catch can intercept.
template <typename T>
T _mlc_load(Schema* schema, const std::string& path) {
    char* errmsg = NULL;
    void* voidstar = mlc_load(path.c_str(), schema, &errmsg);
    if (errmsg != NULL) {
        std::string msg(errmsg);
        free(errmsg);
        throw MorlocException(std::string("@load: ") + msg);
    }
    if (voidstar == NULL) {
        throw MorlocException("@load: failed to load '" + path + "'");
    }
    T* dummy = nullptr;
    T result = from_voidstar(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
}

// ── Stream-handle wrappers ───────────────────────────────────────────────
// @open: returns a 64-bit handle (kind = MLC_KIND_{IFILE|ISTREAM|OSTREAM}).
inline int64_t _mlc_open(const std::string& path, uint8_t kind) {
    char* errmsg = NULL;
    int64_t handle = mlc_open(path.c_str(), kind, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return handle;
}
// @close: returns void; the runtime bumps generation and frees the slot.
inline void _mlc_close(int64_t handle) {
    char* errmsg = NULL;
    mlc_close(handle, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
}

// _mlc_throw returns a universal-conversion helper so that
// `<cond> ? _mlc_throw(msg) : someInt` type-checks in either branch of a
// conditional (both arms of morloc's `?`/`:` lower to lambdas of the same
// return type). The constructor throws, so control never reaches the
// operator T() bodies; std::terminate is just a defence in depth.
struct _MlcThrowHelper {
    template<typename T> operator T() const { std::terminate(); }
};
inline _MlcThrowHelper _mlc_throw(const std::string& msg) {
    throw MorlocException(msg);
}
// @catch: run `fallible()`; on any std::exception, run `fallback()` and
// return its result. Template deduction picks the return type from the
// fallible thunk. The fallback must return the same type.
template<typename FL, typename FB>
auto _mlc_catch(FL&& fallible, FB&& fallback) -> decltype(fallible()) {
    try { return fallible(); }
    catch (const std::exception&) { return fallback(); }
}
// @fschema: read a file's element schema string without opening it.
inline std::string _mlc_fschema(const std::string& path) {
    char* errmsg = NULL;
    char* s = mlc_fschema(path.c_str(), &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    std::string out(s ? s : "");
    if (s) free(s);
    return out;
}
// @flen: total element count of an IFile (`length f`). Cheap; reads
// from the cached StreamDiag at open time.
inline int64_t _mlc_ifile_length(int64_t handle) {
    char* errmsg = NULL;
    int64_t n = mlc_ifile_length(handle, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return n;
}

// @ifile_walk: unified IFile pattern walker. The `path` encodes the
// walk-step chain (".[]", ".[:]", ".1.foo", etc.); `args` carries the
// runtime bracket bounds (Python-style optional ints; absent slots
// take the default). T is the materialized result type the wrapper
// converts the returned voidstar into.
template <typename T>
T _mlc_ifile_walk(
    Schema* schema, int64_t handle,
    const std::string& path,
    std::initializer_list<std::optional<int64_t>> args
) {
    // Pack the optional<int64_t> args into the C-ABI struct array. The
    // struct is fixed-size (16 bytes) so a contiguous vector is the
    // natural marshal target. Empty initializer -> nullptr / n_args=0.
    std::vector<mlc_ifile_walk_arg> packed;
    packed.reserve(args.size());
    for (const auto& a : args) {
        mlc_ifile_walk_arg w{};
        if (a.has_value()) { w.has = 1; w.value = *a; }
        packed.push_back(w);
    }
    char* errmsg = NULL;
    void* voidstar = mlc_ifile_walk(
        handle, path.c_str(),
        packed.empty() ? nullptr : packed.data(),
        (uint64_t)packed.size(),
        &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    T* dummy = nullptr;
    T result = from_voidstar(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
}

// @next: materialise the IStream's current sub-packet as `[a]` and
// advance the cursor. Empty list at EOF.
template <typename T>
T _mlc_next(Schema* schema, int64_t handle) {
    char* errmsg = NULL;
    void* voidstar = mlc_next(handle, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    T* dummy = nullptr;
    T result = from_voidstar(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
}

// @stream: derive an IStream handle from an open IFile handle.
inline int64_t _mlc_stream(int64_t ifile_handle) {
    char* errmsg = NULL;
    int64_t h = mlc_stream(ifile_handle, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

// @open path :: <IO> (OStream T) -- typed open. The schema string for T
// is baked into the codegen and passed alongside the path. We serialise
// the parsed Schema back to its canonical string via libmorloc's
// `schema_to_string` helper.
inline int64_t _mlc_open_ostream(Schema* schema, const std::string& path) {
    char* errmsg = NULL;
    char* s = schema_to_string(schema);
    if (s == NULL) {
        MLC_INTERNAL_ABORT("_mlc_open_ostream: schema_to_string returned NULL (invalid compile-time schema table entry)");
    }
    int64_t h = mlc_open_ostream(s, path.c_str(), &errmsg);
    free(s);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

// @stdin / @stdout / @stderr: nullary intrinsics. Element schema is
// rendered from the parsed Schema and passed to the runtime; the nexus
// owns fd 0/1/2 and services @next / @write over the RPC socket.
inline int64_t _mlc_open_stdin(Schema* schema) {
    char* errmsg = NULL;
    char* s = schema_to_string(schema);
    if (s == NULL) MLC_INTERNAL_ABORT("_mlc_open_stdin: schema_to_string returned NULL");
    int64_t h = mlc_open_stdin(s, &errmsg);
    free(s);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

inline int64_t _mlc_open_stdout(Schema* schema) {
    char* errmsg = NULL;
    char* s = schema_to_string(schema);
    if (s == NULL) MLC_INTERNAL_ABORT("_mlc_open_stdout: schema_to_string returned NULL");
    int64_t h = mlc_open_stdout(s, &errmsg);
    free(s);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

inline int64_t _mlc_open_stderr(Schema* schema) {
    char* errmsg = NULL;
    char* s = schema_to_string(schema);
    if (s == NULL) MLC_INTERNAL_ABORT("_mlc_open_stderr: schema_to_string returned NULL");
    int64_t h = mlc_open_stderr(s, &errmsg);
    free(s);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

// @write: serialize T to voidstar and emit one sub-packet.
template <typename T>
inline void _mlc_write(Schema* schema, int64_t level, const T& value, int64_t handle) {
    char* errmsg = NULL;
    // Stage value into a SHM block via to_voidstar so the runtime can
    // read it as a contiguous voidstar payload. The eval_arena tracks
    // the allocation; we shfree explicitly after the runtime is done.
    size_t bytes = get_shm_size(schema, value);
    void* voidstar = shmalloc_cpp(bytes);
    void* cursor = (uint8_t*)voidstar + schema->width;
    to_voidstar(voidstar, &cursor, schema, value);
    int rc = mlc_write(static_cast<uint8_t>(level), handle, voidstar, &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    if (rc != 0) {
        MLC_INTERNAL_ABORT("mlc_write returned non-zero without setting errmsg (libmorloc contract violation)");
    }
}

// @append: open existing file for append; schema must match.
inline int64_t _mlc_append(Schema* schema, const std::string& path) {
    char* errmsg = NULL;
    char* s = schema_to_string(schema);
    if (s == NULL) {
        MLC_INTERNAL_ABORT("_mlc_append: schema_to_string returned NULL (invalid compile-time schema table entry)");
    }
    int64_t h = mlc_append(s, path.c_str(), &errmsg);
    free(s);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return h;
}

// @concat: byte-level concat of stream files via sendfile.
inline void _mlc_concat(const std::vector<std::string>& paths, const std::string& dest) {
    char* errmsg = NULL;
    std::vector<const char*> raw_paths;
    raw_paths.reserve(paths.size());
    for (const auto& p : paths) {
        raw_paths.push_back(p.c_str());
    }
    int rc = mlc_concat(
        raw_paths.empty() ? nullptr : raw_paths.data(),
        raw_paths.size(),
        dest.c_str(),
        &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    if (rc != 0) {
        MLC_INTERNAL_ABORT("mlc_concat returned non-zero without setting errmsg (libmorloc contract violation)");
    }
}

// @flush: force buffered writes to be emitted as a sub-packet now.
inline void _mlc_flush(int64_t handle) {
    char* errmsg = NULL;
    int rc = mlc_flush(handle, &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    if (rc != 0) {
        MLC_INTERNAL_ABORT("mlc_flush returned non-zero without setting errmsg (libmorloc contract violation)");
    }
}

// @tell: elements written to @stdout so far (its element_count). Never fails.
inline uint64_t _mlc_tell() {
    char* errmsg = NULL;
    uint64_t n = mlc_tell(&errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    return n;
}

// @tmpfile: create a fresh temp file (removed when the pool call ends) and
// return its path. Used by the whole-list with:/render: gather.
inline std::string _mlc_tmpfile() {
    char* errmsg = NULL;
    char* path = mlc_tmpfile(&errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
    std::string result(path);
    free(path);
    return result;
}

// @close on a Str path: unlink a registered temp file. Errors if the path
// was not created by _mlc_tmpfile in this call.
inline void _mlc_unlink_tmp(const std::string& path) {
    char* errmsg = NULL;
    mlc_unlink_tmp(path.c_str(), &errmsg);
    if (errmsg != NULL) { PROPAGATE_ERROR(errmsg) }
}

uint8_t* foreign_call(const char* socket_filename, size_t mid, ...) {
    char* errmsg = NULL;
    va_list args;
    size_t nargs = 0;

    char socket_path[128];
    snprintf(socket_path, sizeof(socket_path), "%s/%s", g_tmpdir, socket_filename);

    // Count arguments (must be NULL-terminated)
    va_start(args, mid);
    while (va_arg(args, uint8_t*) != NULL) nargs++;
    va_end(args);

    // Allocate and populate args array
    const uint8_t** args_array = (const uint8_t**)malloc((nargs + 1) * sizeof(uint8_t*));
    if (!args_array) MLC_INTERNAL_ABORT("malloc failed in foreign_call (OOM)");

    va_start(args, mid);
    for (size_t i = 0; i < nargs; i++) {
        args_array[i] = va_arg(args, uint8_t*);
    }
    args_array[nargs] = NULL;  // Sentinel
    va_end(args);

    // Original logic with variadic args converted to array
    uint8_t* packet = make_morloc_local_call_packet((uint32_t)mid, args_array, nargs, &errmsg);
    if (errmsg != NULL) {
        free(args_array);
        PROPAGATE_ERROR(errmsg)
    }

    pool_mark_busy();
    uint8_t* result = send_and_receive_over_socket(socket_path, packet, &errmsg);
    pool_mark_idle();

    free(packet);

    if (errmsg != NULL) {
        free(args_array);
        PROPAGATE_ERROR(errmsg)
    }

    // If the foreign pool returned a fail packet, surface it as a C++
    // exception. Without this the raw fail-packet bytes get returned to
    // the autogen caller which then tries to deserialize them as data,
    // producing a confusing downstream error or crashing the worker.
    {
        char* fail_check_err = NULL;
        char* fail_msg = get_morloc_data_packet_error_message(
            (const uint8_t*)result, &fail_check_err);
        if (fail_check_err != NULL) { free(fail_check_err); }
        if (fail_msg != NULL) {
            std::string msg(fail_msg);
            free(fail_msg);
            free(result);
            free(args_array);
            throw MorlocException(msg);
        }
    }

    // Incref the result's SHM so the callee's tracker flush won't destroy
    // data we may still need (e.g. forwarded result packets).
    {
        const morloc_packet_header_t* res_header = (const morloc_packet_header_t*)result;
        if (res_header->command.data.source == PACKET_SOURCE_RPTR) {
            size_t relptr = *(size_t*)(result + res_header->offset + sizeof(morloc_packet_header_t));
            char* resolve_err = NULL;
            void* res_voidstar = rel2abs(relptr, &resolve_err);
            if (resolve_err) { free(resolve_err); resolve_err = NULL; }
            if (res_voidstar) {
                char* incref_err = NULL;
                shincref((absptr_t)res_voidstar, &incref_err);
                if (incref_err) { free(incref_err); }
                _shm_tracker.push_back({(absptr_t)res_voidstar, nullptr});
            }
        }
    }

    free(args_array);
    return result;
}



// Debug-trace runtime hooks (defined in libmorloc). Declared here,
// above the AUTO splice points, so codegen-emitted manifold bodies
// see the prototype for morloc_debug_record_frame. drain/flush are
// only called from the in-template handlers below, but the whole
// trio is grouped here for cohesion.
extern "C" char* morloc_debug_drain_frames();
extern "C" void morloc_debug_flush_dispatch();
extern "C" void morloc_debug_record_frame(
    uint32_t midx,
    const char* name,
    const char* srcloc,
    const char* lang,
    const uint8_t** packets,
    const char** schemas,
    size_t n);



// AUTO signatures statements start
// <<<BREAK>>>
// AUTO signatures statements end



// AUTO manifolds statements start
// <<<BREAK>>>
// AUTO manifolds statements end



// AUTO dispatch start
// <<<BREAK>>>
// AUTO dispatch end


// Drain the debug-trace if --debug was compiled in. Concatenates the
// rendered morloc trace with the caught exception's message so the
// fail packet carries both. When --debug is off (or no frames were
// recorded), the drain returns NULL and we forward the message
// unchanged. Every manifold catch also appends its own "  at <name>
// [cpp] (mid=..., srcloc)" line to the message via string concat,
// so non-debug tracebacks still compose across pools.
static uint8_t* make_fail_packet_with_trace(const char* msg) {
    char* trace = morloc_debug_drain_frames();
    if (trace == NULL) {
        return make_fail_packet(msg);
    }
    std::string combined;
    combined.reserve(strlen(msg) + strlen(trace) + 2);
    combined.append(msg);
    combined.append("\n");
    combined.append(trace);
    free(trace);
    return make_fail_packet(combined.c_str());
}

// Wrappers to adapt compiler-generated dispatch functions to pool_dispatch_fn_t.
// These catch C++ exceptions so the C pool_main never sees them.
static uint8_t* cpp_local_dispatch(uint32_t mid, const uint8_t** args,
                                    size_t nargs, void* ctx) {
    (void)nargs; (void)ctx;
    // Free SHM from previous dispatch (result packet consumed by caller)
    _shm_tracker_flush();
    morloc_debug_flush_dispatch();
    try {
        return local_dispatch(mid, args);
    } catch (const std::exception& e) {
        return make_fail_packet_with_trace(e.what());
    } catch (const char* e) {
        // Codegen-emitted manifold catches usually normalize const char*
        // throws into std::runtime_error already, but a foreign helper
        // that throws outside a wrapped body reaches here directly.
        return make_fail_packet_with_trace(e ? e : "An unknown error occurred");
    } catch (const std::string& e) {
        return make_fail_packet_with_trace(e.c_str());
    } catch (...) {
        return make_fail_packet_with_trace("An unknown error occurred");
    }
}

static uint8_t* cpp_remote_dispatch(uint32_t mid, const uint8_t** args,
                                     size_t nargs, void* ctx) {
    (void)nargs; (void)ctx;
    morloc_debug_flush_dispatch();
    try {
        return remote_dispatch(mid, args);
    } catch (const std::exception& e) {
        return make_fail_packet_with_trace(e.what());
    } catch (const char* e) {
        return make_fail_packet_with_trace(e ? e : "An unknown error occurred");
    } catch (const std::string& e) {
        return make_fail_packet_with_trace(e.c_str());
    } catch (...) {
        return make_fail_packet_with_trace("An unknown error occurred");
    }
}


int main(int argc, char* argv[]) {
    // Line-buffer stderr so diagnostic output is not lost on pool shutdown.
    // stdout is left fully buffered for performance (genome-scale piping)
    // and flushed after each job by pool.c.
    setvbuf(stderr, NULL, _IOLBF, 0);

    // Request SIGTERM when the parent (nexus) dies. Without this,
    // SIGKILL on the nexus leaves pool processes orphaned with
    // leaked SHM segments in /dev/shm.
#ifdef __linux__
    prctl(PR_SET_PDEATHSIG, SIGTERM);
#endif

    // Health check: confirm binary links and print version
    if (argc == 2 && std::string(argv[1]) == "--health") {
        std::cout << "{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}" << std::endl;
        return 0;
    }

    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <socket_path> <tmpdir> <shm_basename>\n";
        return 1;
    }

    g_tmpdir = strdup(argv[2]);

    pool_config_t config = {};
    config.local_dispatch = cpp_local_dispatch;
    config.remote_dispatch = cpp_remote_dispatch;
    config.dispatch_ctx = NULL;
    config.concurrency = POOL_THREADS;
    config.initial_workers = 1;
    config.dynamic_scaling = true;

    _init_schemas();
    int result = pool_main(argc, argv, &config);

    free(g_tmpdir);
    return result;
}
