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

// needed for the thread pool
#include <pthread.h>
#include <signal.h>


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

// Transforms a serialized value into a message ready for the socket
template <typename T>
uint8_t* _put_value(const T& value, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    // toAnything writes to the shared memory volume
    void* voidstar = toAnything(schema, value);

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = abs2rel_cpp(voidstar);

    uint8_t* packet = make_standard_data_packet(relptr, schema);

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
    uint8_t* packet = make_morloc_local_call_packet((uint32_t)mid, args_array, nargs, &errmsg);
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



// AUTO dispatch start
// <<<BREAK>>>
// AUTO dispatch end


pthread_mutex_t shutting_down_mutex = PTHREAD_MUTEX_INITIALIZER;
int shutting_down = 0;

int is_shutting_down() {
    int value;
    pthread_mutex_lock(&shutting_down_mutex);
    value = shutting_down;
    pthread_mutex_unlock(&shutting_down_mutex);
    return value;
}

void set_shutting_down(int value) {
    pthread_mutex_lock(&shutting_down_mutex);
    shutting_down = value;
    pthread_mutex_unlock(&shutting_down_mutex);
}

uint8_t* dispatch(const uint8_t* msg){
    char* errmsg = NULL;

    morloc_call_t* call_packet = NULL;
    uint32_t mid = 0;
    const uint8_t** args;

    bool is_ping = packet_is_ping(msg, &errmsg);
    PROPAGATE_FAIL_PACKET(errmsg)

    if(is_ping){
        uint8_t* pong = return_ping(msg, &errmsg);
        PROPAGATE_FAIL_PACKET(errmsg)
        return pong;
    }

    bool is_local_call = packet_is_local_call(msg, &errmsg);
    bool is_remote_call = packet_is_remote_call(msg, &errmsg);
    errmsg = NULL;


    if(is_local_call || is_remote_call){
        call_packet = read_morloc_call_packet(msg, &errmsg);
        mid = call_packet->midx;
        args = (const uint8_t**)call_packet->args;
        free(call_packet);
        try {
            if(is_local_call){
                return local_dispatch(mid, args);
            } else {
                return remote_dispatch(mid, args);
            }
        } catch (const std::exception& e) {
            // Wrap any exceptions in a failing data packet
            return make_fail_packet(e.what());
        } catch (...) {
            // Wrap any unexpected exceptions in failing data packet
            return make_fail_packet("An unknown error occurred");
        }

    }

    if(!is_local_call){
        if(errmsg != NULL) {
            return make_fail_packet(errmsg);
        } else {
            return make_fail_packet("In C++ pool, call failed due to inappropriate packet");
        }
    }


    return make_fail_packet("No manifold found");
}


typedef struct job_s {
    int client_fd;
    struct job_s* next;
} job_t;

typedef struct job_queue_s {
    job_t* head;
    job_t* tail;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
} job_queue_t;


// start an empty job queue
void job_queue_init(job_queue_t* q) {
    q->head = q->tail = NULL;
    pthread_mutex_init(&q->mutex, NULL);
    pthread_cond_init(&q->cond, NULL);
}


// add a new client socket fd to the queue
void job_queue_push(job_queue_t* q, int client_fd) {
    job_t* job = (job_t*)malloc(sizeof(job_t));
    job->client_fd = client_fd;
    job->next = NULL;
    pthread_mutex_lock(&q->mutex);
    if (q->tail) {
        q->tail->next = job;
        q->tail = job;
    } else {
        q->head = job;
        q->tail = job;
    }
    // signal that a job is ready, one worker will be awoken
    pthread_cond_signal(&q->cond);
    pthread_mutex_unlock(&q->mutex);
}


// remove and return the oldest element as soon as one is available
int job_queue_pop(job_queue_t* q) {
    pthread_mutex_lock(&q->mutex);

    while (!q->head && !is_shutting_down()) {
        // wati for job_queue_push to add a new job (this releases the mutex
        // until the condition is met)
        pthread_cond_wait(&q->cond, &q->mutex);
    }
    if (is_shutting_down()) {
        pthread_mutex_unlock(&q->mutex);
        pthread_exit(NULL);
    }
    job_t* job = q->head;
    q->head = job->next;
    if (!q->head) q->tail = NULL;
    int fd = job->client_fd;
    free(job);
    pthread_mutex_unlock(&q->mutex);
    return fd;
}


void* worker_thread(void* arg) {
    job_queue_t* q = (job_queue_t*)arg;
    while (!is_shutting_down()) {
        // takes a job from the queue as soon as one is available
        int client_fd = job_queue_pop(q);

        char* errmsg = NULL;
        uint8_t* result = NULL;
        size_t bytes_sent = 0;
        size_t length = 0;

        // Pull data from the client
        uint8_t* client_data = stream_from_client(client_fd, &errmsg);
        if(errmsg != NULL){
            std::cerr << "Failed to read client data: " << errmsg << std::endl;
            close_socket(client_fd);
            continue;
        }
        if(client_data == NULL){
            std::cerr << "Retrieved NULL client result: " << std::endl;
            close_socket(client_fd);
            continue;
        }

        // Fail if no data was pulled
        length = morloc_packet_size(client_data, &errmsg);
        if(errmsg != NULL){
            std::cerr << "Malformed packet: " << errmsg << std::endl;
            close_socket(client_fd);
            continue;
        } else if (length == 0) {
            std::cerr << "Zero length packet received from client" << std::endl;
            close_socket(client_fd);
            continue;
        }

        // Run the job
        try {
            // failure in the user code must be caught gracefully and passed forward
            result = dispatch(client_data);
        } catch (const std::exception& e) {
            result = make_fail_packet(e.what());
        }

        // return the result to the client and move on
        // do not wait for the client to finish processing
        bytes_sent = send_packet_to_foreign_server(client_fd, result, &errmsg);
        if(errmsg != NULL){
            std::cerr << "Failed to send data: " << errmsg << std::endl;
            close_socket(client_fd);
            continue;
        }

        // close the child file descriptor
        close_socket(client_fd);
    }
    return NULL;
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

    job_queue_t queue;
    job_queue_init(&queue);

    long nthreads = sysconf(_SC_NPROCESSORS_ONLN);
    pthread_t threads[nthreads];
    for (int i = 0; i < nthreads; ++i) {
        pthread_create(&threads[i], NULL, worker_thread, &queue);
    }

    while (!is_shutting_down()) {
        int client_fd = wait_for_client_with_timeout(daemon, 10000, &errmsg);
        if (errmsg != NULL){

            std::cerr << "Failed to read client:\n" << errmsg << std::endl;
            errmsg = NULL;
        }
        if( client_fd > 0 ){
            job_queue_push(&queue, client_fd);
        }
    }

    // set_shutting_down(1);
    // // tell all the workers that it is time to die
    // pthread_cond_broadcast(&q->cond);

    if(daemon != NULL){
        close_daemon(&daemon);
    }

    free(g_tmpdir);

    for (int i = 0; i < nthreads; ++i) {
        pthread_join(threads[i], NULL);
    }

    return 0;
}
