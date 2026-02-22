#include "morloc.h"
#include <pthread.h>

// ---- Internal types ----

typedef struct pool_job_s {
    int client_fd;
    struct pool_job_s* next;
} pool_job_t;

typedef struct pool_job_queue_s {
    pool_job_t* head;
    pool_job_t* tail;
    pthread_mutex_t mutex;
    pthread_cond_t cond;
} pool_job_queue_t;

// Shared mmap counter (for POOL_FORK busy tracking across processes)
typedef struct {
    volatile int value;
} pool_shared_counter_t;

struct pool_state_s {
    pool_config_t config;
    language_daemon_t* daemon;
    char* tmpdir;

    // POOL_THREADS
    pool_job_queue_t queue;

    // POOL_FORK
    int job_read_fd;   // worker reads fds from here
    int job_write_fd;  // dispatcher writes fds here
    int* child_pids;
    int child_count;

    // Busy tracking for POOL_THREADS (accessed via __atomic builtins)
    int busy_count;
    int total_workers;

    // Busy tracking for POOL_FORK (shared mmap, survives fork)
    pool_shared_counter_t* shared_busy_count;

    volatile sig_atomic_t shutting_down;
};

// Global pool state for pool_mark_busy/idle and signal handler
// volatile pointer: read from signal handler, must not be optimized away
static pool_state_t* volatile g_pool_state = NULL;

// ---- Busy tracking ----

void pool_mark_busy(void) {
    if (!g_pool_state) return;
    if (g_pool_state->shared_busy_count) {
        __atomic_add_fetch(&g_pool_state->shared_busy_count->value, 1, __ATOMIC_RELAXED);
    } else {
        __atomic_add_fetch(&g_pool_state->busy_count, 1, __ATOMIC_RELAXED);
    }
}

void pool_mark_idle(void) {
    if (!g_pool_state) return;
    if (g_pool_state->shared_busy_count) {
        __atomic_sub_fetch(&g_pool_state->shared_busy_count->value, 1, __ATOMIC_RELAXED);
    } else {
        __atomic_sub_fetch(&g_pool_state->busy_count, 1, __ATOMIC_RELAXED);
    }
}

// ---- Signal handling ----

static void pool_sigterm_handler(int sig) {
    (void)sig;
    if (g_pool_state) {
        g_pool_state->shutting_down = 1;
    }
}

// ---- Packet dispatch ----

uint8_t* pool_dispatch_packet(
    const uint8_t* packet,
    pool_dispatch_fn_t local_dispatch,
    pool_dispatch_fn_t remote_dispatch,
    void* ctx
) {
    char* errmsg = NULL;

    if (!packet) {
        return make_fail_packet("NULL packet in pool dispatch");
    }

    bool is_ping_pkt = packet_is_ping(packet, &errmsg);
    if (errmsg) goto fail;

    if (is_ping_pkt) {
        uint8_t* pong = return_ping(packet, &errmsg);
        if (errmsg) goto fail;
        return pong;
    }

    bool is_local = packet_is_local_call(packet, &errmsg);
    if (errmsg) goto fail;

    bool is_remote = packet_is_remote_call(packet, &errmsg);
    if (errmsg) goto fail;

    if (is_local || is_remote) {
        morloc_call_t* call = read_morloc_call_packet(packet, &errmsg);
        if (errmsg) goto fail;

        uint32_t mid = call->midx;
        const uint8_t** args = (const uint8_t**)call->args;
        size_t nargs = call->nargs;

        pool_dispatch_fn_t fn = is_local ? local_dispatch : remote_dispatch;
        uint8_t* result = fn(mid, args, nargs, ctx);

        free_morloc_call(call);

        if (!result) {
            return make_fail_packet("dispatch callback returned NULL");
        }
        return result;
    }

    return make_fail_packet("Unexpected packet type in pool dispatch");

fail:
    {
        uint8_t* fail_pkt = make_fail_packet(errmsg);
        free(errmsg);
        return fail_pkt;
    }
}

// ---- Job queue (POOL_THREADS) ----

static void pool_queue_init(pool_job_queue_t* q) {
    q->head = q->tail = NULL;
    pthread_mutex_init(&q->mutex, NULL);
    pthread_cond_init(&q->cond, NULL);
}

static void pool_queue_push(pool_job_queue_t* q, int client_fd) {
    pool_job_t* job = (pool_job_t*)malloc(sizeof(pool_job_t));
    if (!job) {
        fprintf(stderr, "malloc failed in pool_queue_push, dropping client\n");
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
    pthread_cond_signal(&q->cond);
    pthread_mutex_unlock(&q->mutex);
}

// Returns client_fd, or -1 on shutdown
static int pool_queue_pop(pool_state_t* state) {
    pool_job_queue_t* q = &state->queue;

    pthread_mutex_lock(&q->mutex);
    while (!q->head && !state->shutting_down) {
        struct timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);
        ts.tv_nsec += 100000000; // 100ms
        if (ts.tv_nsec >= 1000000000) {
            ts.tv_sec += 1;
            ts.tv_nsec -= 1000000000;
        }
        pthread_cond_timedwait(&q->cond, &q->mutex, &ts);
    }
    if (state->shutting_down) {
        pthread_mutex_unlock(&q->mutex);
        return -1;
    }

    pool_job_t* job = q->head;
    q->head = job->next;
    if (!q->head) q->tail = NULL;
    int fd = job->client_fd;
    free(job);
    pthread_mutex_unlock(&q->mutex);
    return fd;
}

// ---- Worker thread (POOL_THREADS) ----

static void* pool_worker_thread(void* arg) {
    pool_state_t* state = (pool_state_t*)arg;

    while (!state->shutting_down) {
        int client_fd = pool_queue_pop(state);
        if (client_fd < 0) break;

        char* errmsg = NULL;

        uint8_t* client_data = stream_from_client(client_fd, &errmsg);
        if (!client_data || errmsg) {
            if (errmsg) {
                fprintf(stderr, "Failed to read client data: %s\n", errmsg);
                free(errmsg);
                errmsg = NULL;
            }
            free(client_data);
            close_socket(client_fd);
            continue;
        }

        size_t length = morloc_packet_size(client_data, &errmsg);
        if (errmsg) {
            fprintf(stderr, "Malformed packet: %s\n", errmsg);
            free(errmsg);
            errmsg = NULL;
            free(client_data);
            close_socket(client_fd);
            continue;
        }
        if (length == 0) {
            fprintf(stderr, "Zero length packet received from client\n");
            free(client_data);
            close_socket(client_fd);
            continue;
        }

        uint8_t* result = pool_dispatch_packet(
            client_data,
            state->config.local_dispatch,
            state->config.remote_dispatch,
            state->config.dispatch_ctx
        );
        free(client_data);

        if (result) {
            send_packet_to_foreign_server(client_fd, result, &errmsg);
            free(result);
            if (errmsg) {
                fprintf(stderr, "Failed to send data: %s\n", errmsg);
                free(errmsg);
                errmsg = NULL;
            }
        }

        close_socket(client_fd);
    }
    return NULL;
}

// ---- POOL_THREADS main ----

static int pool_main_threads(pool_state_t* state, char* socket_path,
                              char* tmpdir, char* shm_basename) {
    char* errmsg = NULL;

    state->daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &errmsg);
    if (errmsg) {
        fprintf(stderr, "Failed to start language server:\n%s\n", errmsg);
        free(errmsg);
        return 1;
    }

    state->tmpdir = strdup(tmpdir);
    pool_queue_init(&state->queue);

    int nthreads = state->config.initial_workers;
    int thread_capacity = nthreads + 16; // room for dynamic spawning
    pthread_t* threads = (pthread_t*)calloc(thread_capacity, sizeof(pthread_t));
    if (!threads) {
        fprintf(stderr, "Failed to allocate thread array\n");
        close_daemon(&state->daemon);
        free(state->tmpdir);
        return 1;
    }
    int thread_count = 0;

    for (int i = 0; i < nthreads; i++) {
        int rc = pthread_create(&threads[i], NULL, pool_worker_thread, state);
        if (rc != 0) {
            fprintf(stderr, "pthread_create failed: %s\n", strerror(rc));
            break;
        }
        thread_count++;
    }
    __atomic_store_n(&state->total_workers, thread_count, __ATOMIC_RELAXED);

    // Accept loop
    while (!state->shutting_down) {
        int client_fd = wait_for_client_with_timeout(state->daemon, 10000, &errmsg);
        if (errmsg) {
            fprintf(stderr, "Failed to read client:\n%s\n", errmsg);
            free(errmsg);
            errmsg = NULL;
        }
        if (client_fd > 0) {
            pool_queue_push(&state->queue, client_fd);
        }

        // Dynamic worker spawning
        if (state->config.dynamic_scaling) {
            int busy = __atomic_load_n(&state->busy_count, __ATOMIC_RELAXED);
            int total = __atomic_load_n(&state->total_workers, __ATOMIC_RELAXED);
            if (busy >= total) {
                // Grow thread array if needed
                if (thread_count >= thread_capacity) {
                    int new_cap = thread_capacity * 2;
                    pthread_t* new_threads = (pthread_t*)realloc(threads, new_cap * sizeof(pthread_t));
                    if (new_threads) {
                        threads = new_threads;
                        thread_capacity = new_cap;
                    } else {
                        continue; // skip spawning, try again later
                    }
                }

                int rc = pthread_create(&threads[thread_count], NULL, pool_worker_thread, state);
                if (rc == 0) {
                    thread_count++;
                    __atomic_add_fetch(&state->total_workers, 1, __ATOMIC_RELAXED);
                }
            }
        }
    }

    // Shutdown: wake all workers
    state->shutting_down = 1;
    pthread_cond_broadcast(&state->queue.cond);

    for (int i = 0; i < thread_count; i++) {
        pthread_join(threads[i], NULL);
    }

    // Drain remaining jobs
    while (state->queue.head) {
        pool_job_t* job = state->queue.head;
        state->queue.head = job->next;
        close_socket(job->client_fd);
        free(job);
    }

    if (state->daemon) {
        close_daemon(&state->daemon);
    }

    free(state->tmpdir);
    free(threads);

    pthread_mutex_destroy(&state->queue.mutex);
    pthread_cond_destroy(&state->queue.cond);

    return 0;
}

// ---- fd-passing helpers (for POOL_FORK) ----

static int pool_send_fd(int sock, int fd) {
    struct msghdr msg = {0};
    struct iovec iov;
    char buf[1] = {0};
    char cmsg_buf[CMSG_SPACE(sizeof(int))];

    iov.iov_base = buf;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);

    struct cmsghdr* cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(cmsg), &fd, sizeof(int));

    return sendmsg(sock, &msg, 0) >= 0 ? 0 : -1;
}

static int pool_recv_fd(int sock) {
    struct msghdr msg = {0};
    struct iovec iov;
    char buf[1];
    char cmsg_buf[CMSG_SPACE(sizeof(int))];

    iov.iov_base = buf;
    iov.iov_len = 1;
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);

    ssize_t n = recvmsg(sock, &msg, 0);
    if (n <= 0) return -1;

    struct cmsghdr* cmsg = CMSG_FIRSTHDR(&msg);
    if (!cmsg || cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS) {
        return -1;
    }

    int fd;
    memcpy(&fd, CMSG_DATA(cmsg), sizeof(int));
    return fd;
}

// ---- Shared mmap counter (for POOL_FORK) ----

static pool_shared_counter_t* pool_shared_counter_create(void) {
    pool_shared_counter_t* counter = (pool_shared_counter_t*)mmap(
        NULL, sizeof(pool_shared_counter_t),
        PROT_READ | PROT_WRITE,
        MAP_SHARED | MAP_ANONYMOUS, -1, 0
    );
    if (counter == MAP_FAILED) return NULL;
    counter->value = 0;
    return counter;
}

static void pool_shared_counter_destroy(pool_shared_counter_t* counter) {
    if (counter && counter != MAP_FAILED) {
        munmap(counter, sizeof(pool_shared_counter_t));
    }
}

// ---- POOL_FORK worker ----

static void pool_fork_worker(pool_state_t* state, int read_fd,
                              const char* shm_basename, size_t volume_index) {
    char* errmsg = NULL;

    // Each fork'd worker gets its own SHM volume
    shinit(shm_basename, volume_index, 0xffff, &errmsg);
    if (errmsg) {
        fprintf(stderr, "Worker shinit failed: %s\n", errmsg);
        free(errmsg);
        return;
    }

    while (!state->shutting_down) {
        // Wait for a file descriptor from the dispatcher
        struct pollfd pfd = { .fd = read_fd, .events = POLLIN };
        int ready = poll(&pfd, 1, 100); // 100ms timeout for shutdown check
        if (ready <= 0) continue;

        int client_fd = pool_recv_fd(read_fd);
        if (client_fd < 0) break;

        uint8_t* client_data = stream_from_client(client_fd, &errmsg);
        if (!client_data || errmsg) {
            if (errmsg) { free(errmsg); errmsg = NULL; }
            free(client_data);
            close_socket(client_fd);
            continue;
        }

        uint8_t* result = pool_dispatch_packet(
            client_data,
            state->config.local_dispatch,
            state->config.remote_dispatch,
            state->config.dispatch_ctx
        );
        free(client_data);

        if (result) {
            send_packet_to_foreign_server(client_fd, result, &errmsg);
            free(result);
            if (errmsg) { free(errmsg); errmsg = NULL; }
        }

        close_socket(client_fd);
    }
}

// ---- POOL_FORK main ----

static int pool_main_fork(pool_state_t* state, char* socket_path,
                           char* tmpdir, char* shm_basename) {
    char* errmsg = NULL;

    state->daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &errmsg);
    if (errmsg) {
        fprintf(stderr, "Failed to start language server:\n%s\n", errmsg);
        free(errmsg);
        return 1;
    }

    state->tmpdir = strdup(tmpdir);

    // Shared job queue: dispatcher writes fds to write end, workers read from read end
    int sv[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0) {
        fprintf(stderr, "socketpair failed: %s\n", strerror(errno));
        close_daemon(&state->daemon);
        free(state->tmpdir);
        return 1;
    }
    state->job_read_fd = sv[0];
    state->job_write_fd = sv[1];

    // Shared busy counter via mmap (survives fork)
    state->shared_busy_count = pool_shared_counter_create();
    if (!state->shared_busy_count) {
        fprintf(stderr, "Failed to create shared counter\n");
        close(sv[0]); close(sv[1]);
        close_daemon(&state->daemon);
        free(state->tmpdir);
        return 1;
    }

    int nworkers = state->config.initial_workers;
    int child_capacity = nworkers + 16;
    state->child_pids = (int*)calloc(child_capacity, sizeof(int));
    state->child_count = 0;

    for (int i = 0; i < nworkers; i++) {
        pid_t pid = fork();
        if (pid < 0) {
            fprintf(stderr, "fork failed: %s\n", strerror(errno));
            break;
        }
        if (pid == 0) {
            // Child: close write end, run worker loop
            close(state->job_write_fd);
            // Detach from daemon socket so only parent accepts connections
            close(state->daemon->server_fd);
            state->daemon->server_fd = -1;

            // Language-specific post-fork cleanup (e.g. PyOS_AfterFork_Child)
            if (state->config.post_fork_child) {
                state->config.post_fork_child(state->config.dispatch_ctx);
            }

            pool_fork_worker(state, state->job_read_fd, shm_basename, (size_t)(i + 1));

            close(state->job_read_fd);
            _exit(0);
        }
        // Parent
        if (state->child_count >= child_capacity) {
            child_capacity *= 2;
            state->child_pids = (int*)realloc(state->child_pids, child_capacity * sizeof(int));
        }
        state->child_pids[state->child_count++] = (int)pid;
    }
    state->total_workers = state->child_count;

    // Accept loop
    while (!state->shutting_down) {
        int client_fd = wait_for_client_with_timeout(state->daemon, 10000, &errmsg);
        if (errmsg) {
            fprintf(stderr, "Failed to read client:\n%s\n", errmsg);
            free(errmsg);
            errmsg = NULL;
        }
        if (client_fd > 0) {
            if (pool_send_fd(state->job_write_fd, client_fd) < 0) {
                fprintf(stderr, "Failed to dispatch job fd: %s\n", strerror(errno));
            }
            close_socket(client_fd); // parent's copy
        }

        // Reap dead children (non-blocking)
        for (int i = 0; i < state->child_count; i++) {
            if (state->child_pids[i] > 0) {
                int wstatus;
                pid_t ret = waitpid(state->child_pids[i], &wstatus, WNOHANG);
                if (ret > 0) {
                    state->child_pids[i] = -1; // mark as reaped
                    state->total_workers--;
                }
            }
        }

        // Dynamic worker spawning
        if (state->config.dynamic_scaling &&
            __atomic_load_n(&state->shared_busy_count->value, __ATOMIC_RELAXED) >= state->total_workers) {
            int next_vol = state->child_count + 1;
            pid_t pid = fork();
            if (pid < 0) {
                // fork failed, ignore
            } else if (pid == 0) {
                close(state->job_write_fd);
                close(state->daemon->server_fd);
                state->daemon->server_fd = -1;

                if (state->config.post_fork_child) {
                    state->config.post_fork_child(state->config.dispatch_ctx);
                }

                pool_fork_worker(state, state->job_read_fd, shm_basename, (size_t)next_vol);

                close(state->job_read_fd);
                _exit(0);
            } else {
                if (state->child_count >= child_capacity) {
                    child_capacity *= 2;
                    state->child_pids = (int*)realloc(state->child_pids, child_capacity * sizeof(int));
                }
                state->child_pids[state->child_count++] = (int)pid;
                state->total_workers++;
            }
        }
    }

    // Shutdown: kill children
    for (int i = 0; i < state->child_count; i++) {
        if (state->child_pids[i] > 0) {
            kill(state->child_pids[i], SIGTERM);
        }
    }
    for (int i = 0; i < state->child_count; i++) {
        if (state->child_pids[i] > 0) {
            waitpid(state->child_pids[i], NULL, 0);
        }
    }

    close(state->job_read_fd);
    close(state->job_write_fd);
    pool_shared_counter_destroy(state->shared_busy_count);
    state->shared_busy_count = NULL;
    free(state->child_pids);

    if (state->daemon) {
        close_daemon(&state->daemon);
    }
    free(state->tmpdir);

    return 0;
}

// ---- POOL_SINGLE main ----

static int pool_main_single(pool_state_t* state, char* socket_path,
                             char* tmpdir, char* shm_basename) {
    char* errmsg = NULL;

    state->daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &errmsg);
    if (errmsg) {
        fprintf(stderr, "Failed to start language server:\n%s\n", errmsg);
        free(errmsg);
        return 1;
    }

    state->tmpdir = strdup(tmpdir);

    while (!state->shutting_down) {
        int client_fd = wait_for_client_with_timeout(state->daemon, 10000, &errmsg);
        if (errmsg) {
            fprintf(stderr, "Failed to read client:\n%s\n", errmsg);
            free(errmsg);
            errmsg = NULL;
        }
        if (client_fd <= 0) continue;

        uint8_t* client_data = stream_from_client(client_fd, &errmsg);
        if (!client_data || errmsg) {
            if (errmsg) { free(errmsg); errmsg = NULL; }
            free(client_data);
            close_socket(client_fd);
            continue;
        }

        uint8_t* result = pool_dispatch_packet(
            client_data,
            state->config.local_dispatch,
            state->config.remote_dispatch,
            state->config.dispatch_ctx
        );
        free(client_data);

        if (result) {
            send_packet_to_foreign_server(client_fd, result, &errmsg);
            free(result);
            if (errmsg) { free(errmsg); errmsg = NULL; }
        }

        close_socket(client_fd);
    }

    if (state->daemon) {
        close_daemon(&state->daemon);
    }
    free(state->tmpdir);

    return 0;
}

// ---- Entry point ----

int pool_main(int argc, char** argv, pool_config_t* config) {
    if (argc != 4) {
        fprintf(stderr, "Usage: %s <socket_path> <tmpdir> <shm_basename>\n",
                argc > 0 ? argv[0] : "pool");
        return 1;
    }

    pool_state_t state;
    memset(&state, 0, sizeof(state));
    state.config = *config;
    if (state.config.initial_workers <= 0) {
        state.config.initial_workers = 1;
    }

    g_pool_state = &state;

    // Register SIGTERM handler for graceful shutdown
    struct sigaction sa;
    sa.sa_handler = pool_sigterm_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGTERM, &sa, NULL);

    int result;
    switch (config->concurrency) {
        case POOL_THREADS:
            result = pool_main_threads(&state, argv[1], argv[2], argv[3]);
            break;
        case POOL_FORK:
            result = pool_main_fork(&state, argv[1], argv[2], argv[3]);
            break;
        case POOL_SINGLE:
            result = pool_main_single(&state, argv[1], argv[2], argv[3]);
            break;
        default:
            fprintf(stderr, "Unknown pool concurrency mode: %d\n", config->concurrency);
            result = 1;
            break;
    }

    g_pool_state = NULL;
    return result;
}
