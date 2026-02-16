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
#include <atomic>

// needed for the thread pool
#include <pthread.h>
#include <signal.h>


using namespace std;

char* g_tmpdir;

// Dynamic worker spawning: track how many threads are blocked in foreign_call
static std::atomic<int> g_busy_count{0};
static std::atomic<int> g_total_threads{0};

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

    g_busy_count.fetch_add(1, std::memory_order_relaxed);
    uint8_t* result = send_and_receive_over_socket(socket_path, packet, &errmsg);
    g_busy_count.fetch_sub(1, std::memory_order_relaxed);

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

volatile sig_atomic_t shutting_down = 0;
job_queue_t* g_queue = NULL;

int is_shutting_down() {
    return shutting_down;
}

void set_shutting_down(int value) {
    shutting_down = value;
}

void sigterm_handler(int sig) {
    (void)sig;
    shutting_down = 1;
    // Do not call pthread_cond_broadcast here - it is not async-signal-safe.
    // Workers use pthread_cond_timedwait and will notice the flag within 100ms.
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
    PROPAGATE_FAIL_PACKET(errmsg)

    bool is_remote_call = packet_is_remote_call(msg, &errmsg);
    PROPAGATE_FAIL_PACKET(errmsg)

    if(is_local_call || is_remote_call){
        call_packet = read_morloc_call_packet(msg, &errmsg);
        PROPAGATE_FAIL_PACKET(errmsg)
        mid = call_packet->midx;
        args = (const uint8_t**)call_packet->args;

        uint8_t* result = NULL;
        try {
            if(is_local_call){
                result = local_dispatch(mid, args);
            } else {
                result = remote_dispatch(mid, args);
            }
        } catch (const std::exception& e) {
            result = make_fail_packet(e.what());
        } catch (...) {
            result = make_fail_packet("An unknown error occurred");
        }

        free_morloc_call(call_packet);

        return result;
    }

    return make_fail_packet("In C++ pool, call failed due to inappropriate packet");
}


// start an empty job queue
void job_queue_init(job_queue_t* q) {
    q->head = q->tail = NULL;
    pthread_mutex_init(&q->mutex, NULL);
    pthread_cond_init(&q->cond, NULL);
}


// add a new client socket fd to the queue
void job_queue_push(job_queue_t* q, int client_fd) {
    job_t* job = (job_t*)malloc(sizeof(job_t));
    if (!job) {
        std::cerr << "malloc failed in job_queue_push, dropping client" << std::endl;
        close_socket(client_fd);
        return;
    }
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
        // Wait for job_queue_push to add a new job (this releases the mutex
        // until the condition is met). Timedwait is a fallback; shutdown
        // normally wakes workers via pthread_cond_broadcast.
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_nsec += 100000000; // 100ms
        if (ts.tv_nsec >= 1000000000) {
            ts.tv_sec += 1;
            ts.tv_nsec -= 1000000000;
        }
        pthread_cond_timedwait(&q->cond, &q->mutex, &ts);
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

        if(client_data == NULL || errmsg != NULL){
            if (errmsg != NULL) {
                std::cerr << "Failed to read client data: " << errmsg << std::endl;
                free(errmsg);
                errmsg = NULL;
            } else {
                std::cerr << "Retrieved NULL client result" << std::endl;
            }
            free(client_data);
            client_data = NULL;
            close_socket(client_fd);
            continue;
        }

        // Fail if no data was pulled
        length = morloc_packet_size(client_data, &errmsg);
        if(errmsg != NULL){
            free(client_data);
            client_data = NULL;
            std::cerr << "Malformed packet: " << errmsg << std::endl;
            free(errmsg);
            errmsg = NULL;
            close_socket(client_fd);
            continue;
        } else if (length == 0) {
            free(client_data);
            client_data = NULL;
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
        free(client_data);
        client_data = NULL;

        // return the result to the client and move on
        // do not wait for the client to finish processing
        bytes_sent = send_packet_to_foreign_server(client_fd, result, &errmsg);
        free(result);
        result = NULL;
        if(errmsg != NULL){
            std::cerr << "Failed to send data: " << errmsg << std::endl;
            free(errmsg);
            errmsg = NULL;
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

    if(errmsg != NULL){
        std::cerr << "Failed to start language server:\n" << errmsg << std::endl;
        free(errmsg);
        return 1;
    }

    g_tmpdir = strdup(argv[2]);

    job_queue_t queue;
    job_queue_init(&queue);
    g_queue = &queue;

    // Register SIGTERM handler for graceful shutdown
    struct sigaction sa;
    sa.sa_handler = sigterm_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGTERM, &sa, NULL);

    long nthreads = 1;
    std::vector<pthread_t> threads;
    threads.reserve(nthreads);
    for (long i = 0; i < nthreads; ++i) {
        pthread_t t;
        int rc = pthread_create(&t, NULL, worker_thread, &queue);
        if (rc != 0) {
            std::cerr << "pthread_create failed: " << strerror(rc) << std::endl;
            break;
        }
        threads.push_back(t);
    }
    g_total_threads.store((int)threads.size(), std::memory_order_relaxed);

    while (!is_shutting_down()) {
        int client_fd = wait_for_client_with_timeout(daemon, 10000, &errmsg);
        if (errmsg != NULL){

            std::cerr << "Failed to read client:\n" << errmsg << std::endl;
            free(errmsg);
            errmsg = NULL;
        }
        if( client_fd > 0 ){
            job_queue_push(&queue, client_fd);
        }

        // Dynamic worker spawning: if all threads are blocked in foreign_call,
        // spawn a new one so incoming callbacks can still be served.
        if (g_busy_count.load(std::memory_order_relaxed) >= g_total_threads.load(std::memory_order_relaxed)) {
            pthread_t t;
            int rc = pthread_create(&t, NULL, worker_thread, &queue);
            if (rc == 0) {
                threads.push_back(t);
                g_total_threads.fetch_add(1, std::memory_order_relaxed);
            }
        }
    }

    set_shutting_down(1);
    pthread_cond_broadcast(&queue.cond);

    // Join all worker threads before freeing any resources they may reference.
    for (size_t i = 0; i < threads.size(); ++i) {
        pthread_join(threads[i], NULL);
    }

    // Drain any remaining jobs in the queue
    while (queue.head != NULL) {
        job_t* job = queue.head;
        queue.head = job->next;
        close_socket(job->client_fd);
        free(job);
    }

    // Now safe to free resources -- all workers have exited
    if(daemon != NULL){
        close_daemon(&daemon);
    }

    free(g_tmpdir);

    pthread_mutex_destroy(&queue.mutex);
    pthread_cond_destroy(&queue.cond);

    return 0;
}
