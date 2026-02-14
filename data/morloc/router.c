#include "morloc.h"
#include "json.h"
#include "daemon.h"
#include "http.h"
#include "router.h"

#include <arpa/inet.h>
#include <dirent.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

// Shutdown flag (shared with daemon.c via extern or separate instance)
static volatile sig_atomic_t router_shutdown_requested = 0;

static void router_signal_handler(int sig) {
    (void)sig;
    router_shutdown_requested = 1;
}

// ======================================================================
// fdb scanning
// ======================================================================

router_t* router_init(const char* fdb_path, ERRMSG) {
    PTR_RETURN_SETUP(router_t)

    DIR* dir = opendir(fdb_path);
    RAISE_IF(!dir, "Cannot open fdb directory '%s': %s", fdb_path, strerror(errno))

    router_t* router = (router_t*)calloc(1, sizeof(router_t));
    router->fdb_path = strdup(fdb_path);

    // Count .manifest files
    size_t cap = 8;
    router->programs = (router_program_t*)calloc(cap, sizeof(router_program_t));

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        // Check for .manifest extension
        const char* name = entry->d_name;
        size_t name_len = strlen(name);
        if (name_len < 10 || strcmp(name + name_len - 9, ".manifest") != 0) {
            continue;
        }

        if (router->n_programs >= cap) {
            cap *= 2;
            router->programs = (router_program_t*)realloc(
                router->programs, cap * sizeof(router_program_t));
        }

        router_program_t* prog = &router->programs[router->n_programs];
        memset(prog, 0, sizeof(router_program_t));

        // Extract program name (filename without .manifest)
        size_t prog_name_len = name_len - 9;
        prog->name = strndup(name, prog_name_len);

        // Build full path
        size_t path_len = strlen(fdb_path) + 1 + name_len + 1;
        prog->manifest_path = (char*)malloc(path_len);
        snprintf(prog->manifest_path, path_len, "%s/%s", fdb_path, name);

        // Read and parse manifest
        prog->manifest = read_manifest(prog->manifest_path, &CHILD_ERRMSG);
        if (CHILD_ERRMSG != NULL) {
            fprintf(stderr, "router: warning: failed to parse %s: %s\n",
                    prog->manifest_path, CHILD_ERRMSG);
            free(CHILD_ERRMSG);
            CHILD_ERRMSG = NULL;
            free(prog->name);
            free(prog->manifest_path);
            continue;
        }

        prog->daemon_pid = 0;
        // Socket will be set when daemon starts
        snprintf(prog->daemon_socket, sizeof(prog->daemon_socket),
                 "/tmp/morloc-router-%s.sock", prog->name);

        router->n_programs++;
    }

    closedir(dir);

    RAISE_IF(router->n_programs == 0, "No .manifest files found in fdb directory '%s'", fdb_path)

    return router;
}

void router_free(router_t* router) {
    if (!router) return;
    for (size_t i = 0; i < router->n_programs; i++) {
        free(router->programs[i].name);
        free(router->programs[i].manifest_path);
        if (router->programs[i].manifest) {
            free_manifest(router->programs[i].manifest);
        }
        // Kill daemon if running
        if (router->programs[i].daemon_pid > 0) {
            kill(router->programs[i].daemon_pid, SIGTERM);
        }
    }
    free(router->programs);
    free(router->fdb_path);
    free(router);
}

// ======================================================================
// Daemon management
// ======================================================================

bool router_start_program(router_program_t* prog, ERRMSG) {
    BOOL_RETURN_SETUP

    // Find the mim binary path
    char mim_path[512];
    const char* home = getenv("HOME");
    RAISE_IF(!home, "HOME environment variable not set")

    snprintf(mim_path, sizeof(mim_path), "%s/.local/bin/mim", home);

    // Check mim exists
    RAISE_IF(access(mim_path, X_OK) != 0,
             "mim binary not found at %s", mim_path)

    pid_t pid = fork();
    if (pid == 0) {
        // Child: exec mim with --daemon
        setpgid(0, 0);
        execl(mim_path, "mim",
              prog->manifest_path,
              "--daemon",
              "--socket", prog->daemon_socket,
              NULL);
        // If exec fails
        fprintf(stderr, "router: failed to exec mim for %s: %s\n",
                prog->name, strerror(errno));
        _exit(1);
    } else if (pid > 0) {
        prog->daemon_pid = pid;

        // Wait a moment for the daemon to start
        struct timespec ts = { .tv_sec = 0, .tv_nsec = 200000000 }; // 200ms
        nanosleep(&ts, NULL);

        // Check child is still alive
        int status;
        pid_t result = waitpid(pid, &status, WNOHANG);
        if (result == pid) {
            prog->daemon_pid = 0;
            RAISE("Daemon for '%s' exited immediately (status %d)", prog->name, status)
        }

        return true;
    } else {
        RAISE("fork failed: %s", strerror(errno))
    }
}

// ======================================================================
// Request forwarding
// ======================================================================

// Forward a request to a program daemon over its unix socket
// using the length-prefixed JSON protocol
daemon_response_t* router_forward(
    router_t* router,
    const char* program,
    daemon_request_t* request,
    ERRMSG
) {
    PTR_RETURN_SETUP(daemon_response_t)

    // Find program
    router_program_t* prog = NULL;
    for (size_t i = 0; i < router->n_programs; i++) {
        if (strcmp(router->programs[i].name, program) == 0) {
            prog = &router->programs[i];
            break;
        }
    }
    RAISE_IF(!prog, "Unknown program: %s", program)

    // Start daemon if not running
    if (prog->daemon_pid <= 0) {
        TRY(router_start_program, prog);
    }

    // Connect to daemon socket
    int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    RAISE_IF(sock < 0, "Failed to create socket: %s", strerror(errno))

    {
        struct timeval tv = { .tv_sec = 60, .tv_usec = 0 };
        setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
        setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
    }

    struct sockaddr_un addr;
    memset(&addr, 0, sizeof(addr));
    addr.sun_family = AF_UNIX;
    snprintf(addr.sun_path, sizeof(addr.sun_path), "%s", prog->daemon_socket);

    if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        close(sock);
        // Try restarting daemon
        prog->daemon_pid = 0;
        TRY(router_start_program, prog);

        sock = socket(AF_UNIX, SOCK_STREAM, 0);
        RAISE_IF(sock < 0, "Failed to create socket: %s", strerror(errno))

        {
            struct timeval tv = { .tv_sec = 60, .tv_usec = 0 };
            setsockopt(sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
            setsockopt(sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
        }

        if (connect(sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            close(sock);
            RAISE("Failed to connect to daemon for '%s': %s",
                  program, strerror(errno))
        }
    }

    // Serialize request to JSON
    json_buf_t* jb = json_buf_new();
    json_write_obj_start(jb);

    if (request->id) {
        json_write_key(jb, "id");
        json_write_string(jb, request->id);
    }

    json_write_key(jb, "method");
    switch (request->method) {
        case DAEMON_CALL:     json_write_string(jb, "call"); break;
        case DAEMON_DISCOVER: json_write_string(jb, "discover"); break;
        case DAEMON_HEALTH:   json_write_string(jb, "health"); break;
    }

    if (request->command) {
        json_write_key(jb, "command");
        json_write_string(jb, request->command);
    }

    if (request->args_json) {
        json_write_key(jb, "args");
        json_write_raw(jb, request->args_json);
    }

    json_write_obj_end(jb);
    char* req_json = json_buf_finish(jb);
    size_t req_len = strlen(req_json);

    // Send length-prefixed message
    uint8_t len_buf[4];
    len_buf[0] = (uint8_t)((req_len >> 24) & 0xFF);
    len_buf[1] = (uint8_t)((req_len >> 16) & 0xFF);
    len_buf[2] = (uint8_t)((req_len >> 8) & 0xFF);
    len_buf[3] = (uint8_t)(req_len & 0xFF);

    ssize_t n = send(sock, len_buf, 4, MSG_NOSIGNAL);
    if (n != 4) {
        close(sock);
        free(req_json);
        RAISE("Failed to send request length to daemon")
    }

    size_t total_sent = 0;
    while (total_sent < req_len) {
        n = send(sock, req_json + total_sent, req_len - total_sent, MSG_NOSIGNAL);
        if (n <= 0) {
            close(sock);
            free(req_json);
            RAISE("Failed to send request body to daemon")
        }
        total_sent += (size_t)n;
    }
    free(req_json);

    // Read response: length-prefixed
    uint8_t resp_len_buf[4];
    n = recv(sock, resp_len_buf, 4, MSG_WAITALL);
    if (n != 4) {
        close(sock);
        RAISE("Failed to read response length from daemon")
    }

    uint32_t resp_len = ((uint32_t)resp_len_buf[0] << 24) |
                        ((uint32_t)resp_len_buf[1] << 16) |
                        ((uint32_t)resp_len_buf[2] << 8)  |
                        ((uint32_t)resp_len_buf[3]);

    char* resp_json = (char*)malloc(resp_len + 1);
    if (!resp_json) {
        close(sock);
        RAISE("Failed to allocate response buffer")
    }

    size_t total_recv = 0;
    while (total_recv < resp_len) {
        n = recv(sock, resp_json + total_recv, resp_len - total_recv, 0);
        if (n <= 0) {
            free(resp_json);
            close(sock);
            RAISE("Failed to read response body from daemon")
        }
        total_recv += (size_t)n;
    }
    resp_json[resp_len] = '\0';
    close(sock);

    // Parse response JSON using proper parser
    daemon_response_t* resp = daemon_parse_response(resp_json, resp_len, errmsg_);
    free(resp_json);
    return resp;
}

// ======================================================================
// Router discovery
// ======================================================================

char* router_build_discovery(router_t* router) {
    json_buf_t* jb = json_buf_new();

    json_write_obj_start(jb);
    json_write_key(jb, "programs");
    json_write_arr_start(jb);

    for (size_t i = 0; i < router->n_programs; i++) {
        router_program_t* prog = &router->programs[i];
        json_write_obj_start(jb);

        json_write_key(jb, "name");
        json_write_string(jb, prog->name);

        json_write_key(jb, "running");
        json_write_bool(jb, prog->daemon_pid > 0 && kill(prog->daemon_pid, 0) == 0);

        if (prog->manifest) {
            json_write_key(jb, "commands");
            json_write_arr_start(jb);
            for (size_t c = 0; c < prog->manifest->n_commands; c++) {
                manifest_command_t* cmd = &prog->manifest->commands[c];
                json_write_obj_start(jb);
                json_write_key(jb, "name");
                json_write_string(jb, cmd->name);
                json_write_key(jb, "type");
                json_write_string(jb, cmd->is_pure ? "pure" : "remote");
                json_write_key(jb, "return_type");
                json_write_string(jb, cmd->return_type);
                json_write_obj_end(jb);
            }
            json_write_arr_end(jb);
        }

        json_write_obj_end(jb);
    }

    json_write_arr_end(jb);
    json_write_obj_end(jb);

    return json_buf_finish(jb);
}

// ======================================================================
// Router HTTP request routing
// ======================================================================

// Route HTTP requests for the router (which adds /programs and /<program>/ prefixes)
static daemon_request_t* router_http_to_request(
    http_request_t* req,
    char** out_program,  // set for per-program requests
    ERRMSG
) {
    PTR_RETURN_SETUP(daemon_request_t)

    daemon_request_t* dreq = (daemon_request_t*)calloc(1, sizeof(daemon_request_t));
    RAISE_IF(!dreq, "Failed to allocate daemon_request_t")

    *out_program = NULL;

    // GET /health
    if (req->method == HTTP_GET && strcmp(req->path, "/health") == 0) {
        dreq->method = DAEMON_HEALTH;
        return dreq;
    }

    // GET /programs - list available programs
    if (req->method == HTTP_GET && strcmp(req->path, "/programs") == 0) {
        dreq->method = DAEMON_DISCOVER;
        return dreq;
    }

    // GET /discover - full discovery across all programs
    if (req->method == HTTP_GET && strcmp(req->path, "/discover") == 0) {
        dreq->method = DAEMON_DISCOVER;
        return dreq;
    }

    // GET /discover/<program> - discovery for one program
    if (req->method == HTTP_GET && strncmp(req->path, "/discover/", 10) == 0) {
        const char* prog_name = req->path + 10;
        if (prog_name[0] != '\0') {
            *out_program = strdup(prog_name);
            dreq->method = DAEMON_DISCOVER;
            return dreq;
        }
    }

    // POST /call/<program>/<command>
    if (req->method == HTTP_POST && strncmp(req->path, "/call/", 6) == 0) {
        const char* rest = req->path + 6;
        const char* slash = strchr(rest, '/');
        if (!slash || slash[1] == '\0') {
            free(dreq);
            RAISE("Expected /call/<program>/<command>")
        }

        *out_program = strndup(rest, (size_t)(slash - rest));
        dreq->method = DAEMON_CALL;
        dreq->command = strdup(slash + 1);

        // Parse body for args
        if (req->body && req->body_len > 0) {
            const char* p = req->body;
            while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;

            if (*p == '[') {
                dreq->args_json = strdup(p);
            } else if (*p == '{') {
                const char* args_key = strstr(p, "\"args\"");
                if (args_key) {
                    args_key += 6;
                    while (*args_key == ' ' || *args_key == ':') args_key++;
                    if (*args_key == '[') {
                        const char* arr_start = args_key;
                        int depth = 0;
                        bool in_string = false;
                        while (*args_key) {
                            if (in_string) {
                                if (*args_key == '\\') { args_key++; if (*args_key) args_key++; continue; }
                                if (*args_key == '"') in_string = false;
                            } else {
                                if (*args_key == '"') in_string = true;
                                else if (*args_key == '[') depth++;
                                else if (*args_key == ']') { depth--; if (depth == 0) { args_key++; break; } }
                            }
                            args_key++;
                        }
                        dreq->args_json = strndup(arr_start, (size_t)(args_key - arr_start));
                    }
                }
            }
        }

        return dreq;
    }

    // OPTIONS (CORS)
    if (req->method == HTTP_OPTIONS) {
        dreq->method = DAEMON_HEALTH;
        return dreq;
    }

    free(dreq);
    RAISE("Unknown router endpoint: %s %s",
          req->method == HTTP_GET ? "GET" : "POST", req->path)
}

// ======================================================================
// Router event loop
// ======================================================================

#define ROUTER_MAX_LISTENERS 3

void router_run(daemon_config_t* config, router_t* router) {
    struct sigaction sa;
    sa.sa_handler = router_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGINT, &sa, NULL);

    struct pollfd fds[ROUTER_MAX_LISTENERS];
    int nfds = 0;

    // For the router, we primarily use HTTP
    if (config->http_port > 0) {
        int http_fd = socket(AF_INET, SOCK_STREAM, 0);
        if (http_fd < 0) {
            fprintf(stderr, "router: failed to create http socket: %s\n", strerror(errno));
            return;
        }
        int opt = 1;
        setsockopt(http_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addr.sin_port = htons((uint16_t)config->http_port);

        if (bind(http_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            fprintf(stderr, "router: failed to bind http port %d: %s\n",
                    config->http_port, strerror(errno));
            close(http_fd);
            return;
        }
        listen(http_fd, 16);

        fds[nfds].fd = http_fd;
        fds[nfds].events = POLLIN;
        nfds++;
        fprintf(stderr, "router: listening on http port %d\n", config->http_port);
    }

    // Unix socket for router
    if (config->unix_socket_path) {
        int sock_fd = socket(AF_UNIX, SOCK_STREAM, 0);
        if (sock_fd < 0) {
            fprintf(stderr, "router: failed to create unix socket: %s\n", strerror(errno));
            return;
        }

        struct sockaddr_un addr;
        memset(&addr, 0, sizeof(addr));
        addr.sun_family = AF_UNIX;
        strncpy(addr.sun_path, config->unix_socket_path, sizeof(addr.sun_path) - 1);
        unlink(config->unix_socket_path);

        if (bind(sock_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            fprintf(stderr, "router: failed to bind unix socket: %s\n", strerror(errno));
            close(sock_fd);
            return;
        }
        listen(sock_fd, 16);

        fds[nfds].fd = sock_fd;
        fds[nfds].events = POLLIN;
        nfds++;
        fprintf(stderr, "router: listening on unix socket %s\n", config->unix_socket_path);
    }

    if (nfds == 0) {
        fprintf(stderr, "router: no listeners configured\n");
        return;
    }

    fprintf(stderr, "router: %zu programs registered\n", router->n_programs);
    for (size_t i = 0; i < router->n_programs; i++) {
        fprintf(stderr, "router:   - %s (%zu commands)\n",
                router->programs[i].name,
                router->programs[i].manifest ? router->programs[i].manifest->n_commands : 0);
    }
    fprintf(stderr, "router: ready\n");

    while (!router_shutdown_requested) {
        int ready = poll(fds, (nfds_t)nfds, 1000);
        if (ready < 0) {
            if (errno == EINTR) continue;
            fprintf(stderr, "router: poll error: %s\n", strerror(errno));
            break;
        }
        if (ready == 0) continue;

        for (int i = 0; i < nfds; i++) {
            if (!(fds[i].revents & POLLIN)) continue;

            int client_fd = accept(fds[i].fd, NULL, NULL);
            if (client_fd < 0) continue;

            {
                struct timeval tv = { .tv_sec = 30, .tv_usec = 0 };
                setsockopt(client_fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
                setsockopt(client_fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
            }

            char* errmsg = NULL;

            http_request_t* http_req = http_parse_request(client_fd, &errmsg);
            if (errmsg) {
                const char* body = "{\"status\":\"error\",\"error\":\"Bad request\"}";
                http_write_response(client_fd, 400, "application/json", body, strlen(body));
                free(errmsg);
                close(client_fd);
                continue;
            }

            char* target_program = NULL;
            daemon_request_t* dreq = router_http_to_request(http_req, &target_program, &errmsg);
            http_free_request(http_req);

            if (errmsg) {
                json_buf_t* jb = json_buf_new();
                json_write_obj_start(jb);
                json_write_key(jb, "status");
                json_write_string(jb, "error");
                json_write_key(jb, "error");
                json_write_string(jb, errmsg);
                json_write_obj_end(jb);
                char* body = json_buf_finish(jb);
                http_write_response(client_fd, 404, "application/json", body, strlen(body));
                free(body);
                free(errmsg);
                close(client_fd);
                continue;
            }

            // Handle router-level requests
            if (!target_program) {
                if (dreq->method == DAEMON_HEALTH) {
                    const char* body = "{\"status\":\"ok\"}";
                    http_write_response(client_fd, 200, "application/json", body, strlen(body));
                } else if (dreq->method == DAEMON_DISCOVER) {
                    char* disco = router_build_discovery(router);
                    http_write_response(client_fd, 200, "application/json", disco, strlen(disco));
                    free(disco);
                }
                daemon_free_request(dreq);
                close(client_fd);
                continue;
            }

            // Per-program request: forward to daemon
            if (dreq->method == DAEMON_DISCOVER) {
                // Find program and return its discovery directly (no daemon needed)
                bool found = false;
                for (size_t p = 0; p < router->n_programs; p++) {
                    if (strcmp(router->programs[p].name, target_program) == 0 &&
                        router->programs[p].manifest) {
                        char* disco = manifest_to_discovery_json(router->programs[p].manifest);
                        http_write_response(client_fd, 200, "application/json",
                                          disco, strlen(disco));
                        free(disco);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    const char* body = "{\"status\":\"error\",\"error\":\"Unknown program\"}";
                    http_write_response(client_fd, 404, "application/json", body, strlen(body));
                }
            } else {
                // Forward call to program daemon
                struct timespec t_start, t_end;
                clock_gettime(CLOCK_MONOTONIC, &t_start);

                daemon_response_t* resp = router_forward(router, target_program, dreq, &errmsg);

                clock_gettime(CLOCK_MONOTONIC, &t_end);
                double duration_ms = (t_end.tv_sec - t_start.tv_sec) * 1000.0
                                   + (t_end.tv_nsec - t_start.tv_nsec) / 1e6;

                if (errmsg) {
                    fprintf(stderr, "router: %s/%s error %.1fms\n",
                            target_program, dreq->command ? dreq->command : "-", duration_ms);
                    json_buf_t* jb = json_buf_new();
                    json_write_obj_start(jb);
                    json_write_key(jb, "status");
                    json_write_string(jb, "error");
                    json_write_key(jb, "error");
                    json_write_string(jb, errmsg);
                    json_write_obj_end(jb);
                    char* body = json_buf_finish(jb);
                    http_write_response(client_fd, 500, "application/json", body, strlen(body));
                    free(body);
                    free(errmsg);
                } else {
                    fprintf(stderr, "router: %s/%s %s %.1fms\n",
                            target_program, dreq->command ? dreq->command : "-",
                            resp->success ? "ok" : "error", duration_ms);
                    size_t resp_len = 0;
                    char* resp_json = daemon_serialize_response(resp, &resp_len);
                    http_write_response(client_fd, resp->success ? 200 : 500,
                                      "application/json", resp_json, resp_len);
                    free(resp_json);
                    daemon_free_response(resp);
                }
            }

            free(target_program);
            daemon_free_request(dreq);
            close(client_fd);
        }
    }

    fprintf(stderr, "router: shutting down\n");

    // Kill all program daemons
    for (size_t i = 0; i < router->n_programs; i++) {
        if (router->programs[i].daemon_pid > 0) {
            kill(router->programs[i].daemon_pid, SIGTERM);
            // Clean up socket files
            unlink(router->programs[i].daemon_socket);
        }
    }

    // Wait for children
    for (size_t i = 0; i < router->n_programs; i++) {
        if (router->programs[i].daemon_pid > 0) {
            waitpid(router->programs[i].daemon_pid, NULL, 0);
        }
    }

    // Close listeners
    for (int i = 0; i < nfds; i++) {
        close(fds[i].fd);
    }

    if (config->unix_socket_path) {
        unlink(config->unix_socket_path);
    }
}
