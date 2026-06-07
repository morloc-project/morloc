#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <algorithm>
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
#include <unistd.h>

#include <limits>
#include <utility>

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

#define PROPAGATE_ERROR(errmsg) \
    if(errmsg != NULL) { \
      char errmsg_buffer[MAX_ERRMSG_SIZE] = { 0 }; \
      snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error C++ pool (%s:%d in %s):\n%s" , __FILE__, __LINE__, __func__, errmsg); \
      free(errmsg); \
      throw std::runtime_error(errmsg_buffer); \
    }

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
        throw std::invalid_argument("First list must have exactly 1 more element than second list");
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

static void _flush_shm_tracker() {
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
        if (!packet) { throw std::runtime_error("Failed to create arrow data packet"); }

        char* err = nullptr;
        void* shm_ptr = rel2abs(relptr, &err);
        if (err) { free(err); }
        if (shm_ptr) { _shm_tracker.push_back({(absptr_t)shm_ptr, nullptr}); }
        return packet;
    } else {
        // Arrow dispatch: schema marker `T` (MORLOC_TABLE) routes through
        // mlc::ArrowTable. The legacy `<arrow>` hint has been retired.
        if (schema->type == MORLOC_TABLE) {
            throw std::runtime_error("Table schema but C++ type is not mlc::ArrowTable");
        }

        void* voidstar = nullptr;
        try {
            voidstar = toAnything(schema, value);
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
            throw std::runtime_error("Arrow data but C++ type is not mlc::ArrowTable");
        }

        // Fast path: inline voidstar -- read directly from packet, no SHM needed
        if (source == PACKET_SOURCE_MESG && format == PACKET_FORMAT_VOIDSTAR) {
            const uint8_t* payload = packet + sizeof(morloc_packet_header_t) + header->offset;
            T* dummy = nullptr;
            return fromAnything(schema, (const void*)payload, dummy, (const void*)payload);
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
        return fromAnything(schema, (void*)voidstar, dummy);
    }
}


// Hash a value, returning a 16-char hex string
template <typename T>
std::string _mlc_hash(const T& value, Schema* schema) {
    void* voidstar = toAnything(schema, value);
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

// Save a value to file in msgpack format
template <typename T>
void _mlc_save(const T& value, Schema* schema, const std::string& path) {
    void* voidstar = toAnything(schema, value);
    char* errmsg = NULL;
    mlc_save(voidstar, schema, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Save a value to file in flat voidstar binary format
template <typename T>
void _mlc_save_voidstar(const T& value, Schema* schema, const std::string& path) {
    void* voidstar = toAnything(schema, value);
    char* errmsg = NULL;
    mlc_save_voidstar(voidstar, schema, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Save a value to file in JSON format
template <typename T>
void _mlc_save_json(const T& value, Schema* schema, const std::string& path) {
    void* voidstar = toAnything(schema, value);
    char* errmsg = NULL;
    mlc_save_json(voidstar, schema, path.c_str(), &errmsg);
    shfree_cpp(voidstar);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
}

// Serialize a value to a JSON string
template <typename T>
std::string _mlc_show(const T& value, Schema* schema) {
    void* voidstar = toAnything(schema, value);
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

// Deserialize a JSON string to a typed value
// Returns std::nullopt on parse failure
template <typename T>
std::optional<T> _mlc_read(Schema* schema, const std::string& json_str) {
    char* errmsg = NULL;
    void* voidstar = mlc_read(json_str.c_str(), schema, &errmsg);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
    if (voidstar == NULL) {
        return std::nullopt;
    }
    T* dummy = nullptr;
    T result = fromAnything(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
}

// Load a value from file, auto-detecting format
// Returns std::nullopt if file does not exist
template <typename T>
std::optional<T> _mlc_load(Schema* schema, const std::string& path) {
    char* errmsg = NULL;
    void* voidstar = mlc_load(path.c_str(), schema, &errmsg);
    if (errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }
    if (voidstar == NULL) {
        return std::nullopt;
    }
    T* dummy = nullptr;
    T result = fromAnything(schema, voidstar, dummy);
    shfree_cpp(voidstar);
    return result;
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
    if (!args_array) throw std::runtime_error("malloc failed in foreign_call");

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
            throw std::runtime_error(msg);
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



// AUTO signatures statements start
// <<<BREAK>>>
// AUTO signatures statements end



// AUTO manifolds statements start
// <<<BREAK>>>
// AUTO manifolds statements end



// AUTO dispatch start
// <<<BREAK>>>
// AUTO dispatch end


// Wrappers to adapt compiler-generated dispatch functions to pool_dispatch_fn_t.
// These catch C++ exceptions so the C pool_main never sees them.
static uint8_t* cpp_local_dispatch(uint32_t mid, const uint8_t** args,
                                    size_t nargs, void* ctx) {
    (void)nargs; (void)ctx;
    // Free SHM from previous dispatch (result packet consumed by caller)
    _flush_shm_tracker();
    try {
        return local_dispatch(mid, args);
    } catch (const std::exception& e) {
        return make_fail_packet(e.what());
    } catch (...) {
        return make_fail_packet("An unknown error occurred");
    }
}

static uint8_t* cpp_remote_dispatch(uint32_t mid, const uint8_t** args,
                                     size_t nargs, void* ctx) {
    (void)nargs; (void)ctx;
    try {
        return remote_dispatch(mid, args);
    } catch (const std::exception& e) {
        return make_fail_packet(e.what());
    } catch (...) {
        return make_fail_packet("An unknown error occurred");
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
