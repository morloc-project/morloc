#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <algorithm> // for std::transform
#include <stdexcept>
#include <fstream>
#include <system_error>
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
        return make_fail_packet(errmsg); \
    } \


// AUTO serialization statements start
// <<<BREAK>>>
// AUTO serialization statements end



// Transforms a serialized value into a message ready for the socket
template <typename T>
uint8_t* _put_value(const T& value, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    // toAnything writes to the shared memory volume
    void* voidstar = toAnything(schema, value);

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = abs2rel_cpp(voidstar);

    uint8_t* packet = make_relptr_data_packet(relptr);

    return packet;
}


// Use a key to retrieve a value
template <typename T>
T _get_value(const uint8_t* packet, const std::string& schema_str){

    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    char* errmsg = NULL;
    uint8_t* voidstar = get_morloc_data_packet_value(packet, schema, &errmsg);
    PROPAGATE_ERROR(errmsg)

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
    if (!args_array) return NULL;

    va_start(args, mid);
    for (size_t i = 0; i < nargs; i++) {
        args_array[i] = va_arg(args, uint8_t*);
    }
    args_array[nargs] = NULL;  // Sentinel
    va_end(args);

    // Original logic with variadic args converted to array
    uint8_t* packet = make_morloc_call_packet((uint32_t)mid, args_array, nargs, &errmsg);
    PROPAGATE_ERROR(errmsg)

    uint8_t* result = send_and_receive_over_socket(socket_path, packet, &errmsg);
    PROPAGATE_ERROR(errmsg)

    free(args_array);
    return result;
}



// AUTO signatures statements start
// <<<BREAK>>>
// AUTO signatures statements end



// AUTO manifolds statements start
// <<<BREAK>>>
// AUTO manifolds statements end



uint8_t* dispatch(const uint8_t* msg){
    char* errmsg = NULL;

    bool is_ping = packet_is_ping(msg, &errmsg);
    PROPAGATE_FAIL_PACKET(errmsg)

    if(is_ping){
        uint8_t* pong = return_ping(msg, &errmsg);
        PROPAGATE_FAIL_PACKET(errmsg)
        return pong;
    }

    bool is_call = packet_is_call(msg, &errmsg);
    if(!is_call){
        if(errmsg != NULL) {
            return make_fail_packet(errmsg);
        } else {
            return make_fail_packet("In C++ pool, call failed due to inappropriate packet");
        }
    }

    morloc_call_t* call_packet = read_morloc_call_packet(msg, &errmsg);
    uint32_t mid = call_packet->midx;
    const uint8_t** args = (const uint8_t**)call_packet->args;
    free(call_packet);

    try {

// AUTO dispatch statements start
// <<<BREAK>>>
// AUTO dispatch statements end

    } catch (const std::exception& e) {
        // Wrap any exceptions in a failing data packet
        return make_fail_packet(e.what());
    } catch (...) {
        // Wrap any unexpected exceptions in failing data packet
        return make_fail_packet("An unknown error occurred");
    }

    return make_fail_packet("No manifold found");
}



// MAIN

int run_job(int client_fd) {
    pid_t pid = fork();
    if (pid == 0) {
        // Enter child process
        //
        char* errmsg = NULL;

        // Pull data from the client
        uint8_t* client_data = stream_from_client(client_fd, &errmsg);
        if(errmsg != NULL){
            std::cerr << "Failed to read client data: " << errmsg << std::endl;
            exit(1);
        }

        // Fail if no data was pulled
        size_t length = morloc_packet_size(client_data, &errmsg);
        if(errmsg != NULL){
            close_socket(client_fd);
            std::cerr << "Malformed packet: " << errmsg << std::endl;
            exit(1);
        } else if (length == 0) {
            close_socket(client_fd);
            std::cerr << "Zero length packet received from client" << std::endl;
            exit(1);
        }

        // Run the job
        uint8_t* result = NULL;
        try {
            result = dispatch(client_data);
        } catch (const std::exception& e) {
            result = make_fail_packet(e.what());
        }

        // return the result to the client and move on
        // do not wait for the client to finish processing
        size_t bytes_sent = send_packet_to_foreign_server(client_fd, result, &errmsg);
        if(errmsg != NULL){
            std::cerr << "Failed to send data: " << errmsg << std::endl;
            exit(1);
        }

        // close the child file descriptor
        close_socket(client_fd);

        // And exit the child
        exit(0);
    } else if (pid > 0) {
        // Enter the parent process

        // immediately close the parent copy of the file descriptor
        close_socket(client_fd);

        return pid; // success
    } else {
        // fork failed
        close_socket(client_fd);
    }

    return -1; // failure
}


int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <socket_path>" << " <tmpdir>" << "<shm_basename>\n";
        return 1;
    }

    char* errmsg = NULL;

    language_daemon_t* daemon = start_daemon(
        argv[1], // path to the socket file
        argv[2], // main temporary directory
        argv[3], // basename for the shared memory files (i.e., in /dev/shm)
        0xffff,  // default shm size
        &errmsg
    );

    g_tmpdir = strdup(argv[2]);

    if(errmsg != NULL){
        std::cerr << "Failed to start language server:\n" << errmsg << std::endl;
        return 1;
    }

    while (true) {
        int client_fd = wait_for_client(daemon, &errmsg);
        if (errmsg != NULL){
            std::cerr << "Failed to read client:\n" << errmsg << std::endl;
            errmsg = NULL;
        }
        if (client_fd >= 0){
            run_job(client_fd);
        }
    }

    if(daemon != NULL){
        close_daemon(&daemon);
    }

    free(g_tmpdir);

    return 0;
}
