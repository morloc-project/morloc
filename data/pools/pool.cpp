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

// needed for foreign interface
#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <unistd.h>

#include <limits>
#include <utility>


using namespace std;

char* g_tmpdir;

uint8_t* foreign_call(const char* socket_filename, size_t mid, ...) __attribute__((sentinel));

// AUTO include statements start
// <<<BREAK>>>
// AUTO include statements end

// Proper linking of cppmorloc requires it be included AFTER the custom modules
#include "cppmorloc.hpp"

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

// Thread-local schema cache: avoids re-parsing the same schema strings
Schema* get_cached_schema(const char* schema_str) {
    thread_local std::unordered_map<std::string, Schema*> cache;
    auto it = cache.find(schema_str);
    if (it != cache.end()) return it->second;
    Schema* schema = parse_schema_cpp(schema_str);
    cache[schema_str] = schema;
    return schema;
}

// Transforms a serialized value into a message ready for the socket
template <typename T>
uint8_t* _put_value(const T& value, const std::string& schema_str) {
    Schema* schema = get_cached_schema(schema_str.c_str());

    void* voidstar = nullptr;
    try {
        voidstar = toAnything(schema, value);
        relptr_t relptr = abs2rel_cpp(voidstar);
        return make_standard_data_packet(relptr, schema);
    } catch (...) {
        if (voidstar) shfree_cpp(voidstar);
        throw;
    }
}


// Use a key to retrieve a value
template <typename T>
T _get_value(const uint8_t* packet, const std::string& schema_str){
    Schema* schema = get_cached_schema(schema_str.c_str());

    char* errmsg = NULL;
    uint8_t* voidstar = get_morloc_data_packet_value(packet, schema, &errmsg);
    if(errmsg != NULL) {
        PROPAGATE_ERROR(errmsg)
    }

    T* dumby = nullptr;
    return fromAnything(schema, (void*)voidstar, dumby);
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

    int result = pool_main(argc, argv, &config);

    free(g_tmpdir);
    return result;
}
