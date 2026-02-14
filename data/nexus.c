#include "morloc.h"

#include <errno.h>
#include <getopt.h>
#include <inttypes.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define INITIAL_PING_TIMEOUT_MICROSECONDS 10000
#define INITIAL_RETRY_DELAY 0.001
#define RETRY_MULTIPLIER 1.25
#define MAX_RETRIES 16

#define MAX_DAEMONS 32

// Max getopt options per command (opts + flags + group entries)
#define MAX_OPTIONS 128

#define ERROR(msg, ...) \
    fprintf(stderr, "Error (%s:%d in %s): " msg "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    clean_exit(1);

#define ERROR_WITH(end, msg, ...) \
    fprintf(stderr, "Error (%s:%d in %s): " msg "\n", __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    end; \
    clean_exit(1);

#define ERROR_TRY_GOTO(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &errmsg); \
    if(errmsg != NULL){ \
        fprintf(stderr, "Error (%s:%d in %s):\n%s\n", __FILE__, __LINE__, __func__, errmsg); \
        goto end; \
    }

typedef enum {
    JSON,
    MessagePack,
    VoidStar
} format_enum;

typedef struct config_s {
    int help_flag;
    char* packet_path;
    char* socket_base;
    char* output_path;
    format_enum output_format;
    // daemon/router mode
    int daemon_flag;
    int router_flag;
    char* unix_socket_path;
    int tcp_port;
    int http_port;
    char* fdb_path;
} config_t;

// global pid list of language daemons
// a pid of 0 means unused, -1 means already reaped
pid_t pids[MAX_DAEMONS] = { 0 };

// process group IDs of language daemons - never modified by signal handler
// used in clean_exit() to kill entire process groups even if the leader is already dead
pid_t pgids[MAX_DAEMONS] = { 0 };

// global temporary file
char* tmpdir = NULL;

// SIGCHLD handler: reap terminated children immediately to prevent zombies
void sigchld_handler(int sig) {
    (void)sig;
    int saved_errno = errno;
    pid_t pid;
    int status;
    while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
        for (size_t i = 0; i < MAX_DAEMONS; i++) {
            if (pids[i] == pid) {
                pids[i] = -1; // mark as reaped
                break;
            }
        }
    }
    errno = saved_errno;
}

void clean_exit(int exit_code){
    // Block SIGCHLD during cleanup to avoid races with the handler
    sigset_t block_chld, old_mask;
    sigemptyset(&block_chld);
    sigaddset(&block_chld, SIGCHLD);
    sigprocmask(SIG_BLOCK, &block_chld, &old_mask);

    // Send SIGTERM to all pool process groups
    for (size_t i = 0; i < MAX_DAEMONS; i++) {
        if (pgids[i] > 0) {
            kill(-pgids[i], SIGTERM);
        }
    }

    // Wait for process groups to exit (up to 500ms per group, then SIGKILL)
    for (size_t i = 0; i < MAX_DAEMONS; i++) {
        if (pgids[i] <= 0) continue;

        while (waitpid(-1, NULL, WNOHANG) > 0) {}
        if (kill(-pgids[i], 0) == -1) continue;

        int group_dead = 0;
        for (int attempt = 0; attempt < 50; attempt++) {
            while (waitpid(-1, NULL, WNOHANG) > 0) {}
            if (kill(-pgids[i], 0) == -1) {
                group_dead = 1;
                break;
            }
            struct timespec ts = { .tv_sec = 0, .tv_nsec = 10000000 }; // 10ms
            nanosleep(&ts, NULL);
        }

        if (!group_dead) {
            kill(-pgids[i], SIGKILL);
            struct timespec ts = { .tv_sec = 0, .tv_nsec = 50000000 }; // 50ms
            nanosleep(&ts, NULL);
        }
    }

    // Final reap of any remaining descendants
    while (waitpid(-1, NULL, WNOHANG) > 0) {}

    // Clean up resources (all pools are dead now)
    if (tmpdir != NULL) {
        delete_directory(tmpdir);
        free(tmpdir);
        tmpdir = NULL;
    }

    char* errmsg = NULL;
    shclose(&errmsg);
    if(errmsg != NULL){
        fprintf(stderr, "shclose error: %s\n", errmsg);
        exit(1);
    }

    exit(exit_code);
}

// make a hash from a seed, the process id, nanoseconds since the epoch
uint64_t make_job_hash(uint64_t seed) {
    uint64_t pid = (uint64_t)getpid();

    struct timespec ts;

    // Get epoch time
    clock_gettime(CLOCK_REALTIME, &ts);
    uint64_t epoch_ns = (uint64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;

    // Get monotonic time
    clock_gettime(CLOCK_MONOTONIC, &ts);
    uint64_t uptime_ns = (uint64_t)ts.tv_sec * 1000000000 + ts.tv_nsec;

    char buffer[64] = {0};
    snprintf(buffer, sizeof(buffer),
            "%" PRIu64 ":%" PRIu64 ":%" PRIu64,
            pid, epoch_ns, uptime_ns);

    return morloc_xxh64(buffer, strlen(buffer), seed);
}


char* make_tmpdir(ERRMSG) {
    PTR_RETURN_SETUP(char)

    char template[] = "/tmp/morloc.XXXXXX";

    char* temp_dir = mkdtemp(template);
    RAISE_IF(temp_dir == NULL, "Failed to create temporary directory")

    // strdup since template is stack-local
    char* result = strdup(temp_dir);
    RAISE_IF(result == NULL, "Failed to allocate temporary directory path")

    return result;
}


int start_language_server(const morloc_socket_t* socket, ERRMSG){
    INT_RETURN_SETUP

    pid_t pid = fork();

    if (pid == 0) { // Child process
        setpgid(0, 0); // New process group so we can kill all children
        execvp(socket->syscmd[0], socket->syscmd);
        RAISE("execvp failed: %s", strerror(errno)) // Only reached if exec fails
    } else if (pid > 0) { // Parent process
        setpgid(pid, pid); // ensure child is in its own group (races with child's setpgid)
        return pid;
    } else {
        RAISE("fork failed: %s", strerror(errno));
    }
}


void print_return(uint8_t* packet, Schema* schema, config_t config){
    char* errmsg = NULL;
    char* child_errmsg = NULL;

    char* packet_error = get_morloc_data_packet_error_message(packet, &child_errmsg);
    if(packet_error != NULL){
        ERROR("Run failed\n%s", packet_error)
    }
    if(child_errmsg != NULL && packet_error != NULL){
        ERROR("Internal error\n%s", packet_error)
    }
    if(child_errmsg != NULL){
        ERROR("Internal error")
    }

    uint8_t* packet_value = get_morloc_data_packet_value(packet, schema, &child_errmsg);
    if(child_errmsg != NULL){
        ERROR("%s", child_errmsg);
    }

    if(config.output_format == JSON){
        // print result
        print_voidstar(packet_value, schema, &child_errmsg);
    } else if (config.output_format == MessagePack) {

        char* mpk_ptr = NULL; // MessagePack data point
        size_t mpk_size = 0;

        // translate returned data to MessagePack format
        ERROR_TRY_GOTO(pack_with_schema, (void*)packet_value, schema, &mpk_ptr, &mpk_size);

        // print MessagePack data to STDOUT
        ERROR_TRY_GOTO(print_binary, mpk_ptr, mpk_size);

    } else if (config.output_format == VoidStar) {

        // print Morloc packet
        ERROR_TRY_GOTO(print_morloc_data_packet, packet, schema);

    } else {
        ERROR("Unsupported output_format specified (this should be impossible, this error message indicates a bug in the code).");
    }

    if(child_errmsg == NULL){
        clean_exit(0);
    } else {
        ERROR("Failed to print return packet\n%s", child_errmsg);
    }

end:
    clean_exit(0);
}


void start_daemons(morloc_socket_t** all_sockets){
    char* errmsg = NULL;

    size_t npids = 0;
    for(size_t i = 0; all_sockets[i] != NULL; i++){
        npids++;
        all_sockets[i]->pid = start_language_server(all_sockets[i], &errmsg);
        // add pid to global list for later cleanup
        pids[i] = all_sockets[i]->pid;
        pgids[i] = all_sockets[i]->pid; // pgid == pid due to setpgid(0, 0)
        if(errmsg != NULL){
            ERROR("%s", errmsg);
        }
    }

    // wait for everything to wake up
    for(size_t si = 0; all_sockets[si] != NULL; si++){
        morloc_socket_t* sock = all_sockets[si];
        uint8_t* ping_packet = make_ping_packet();
        double retry_time = INITIAL_RETRY_DELAY;
        int ping_timeout = INITIAL_PING_TIMEOUT_MICROSECONDS;
        uint8_t* return_data;
        pid_t child_pid = sock->pid;

        for(int attempt = 0; attempt <= MAX_RETRIES; attempt++){

            // Check if SIGCHLD handler already reaped this child
            if (pids[si] == -1) {
                ERROR("Child process with pid %d for socket '%s' died unexpectedly (reaped by signal handler).", child_pid, sock->socket_filename);
            }

            int status = 0;
            pid_t wait_result = waitpid(child_pid, &status, WNOHANG);
            if (wait_result == child_pid) {
                pids[si] = -1;
                ERROR("Child process with pid %d for socket '%s' died unexpectedly (status: %d).", child_pid, sock->socket_filename, status);
            } else if (wait_result == -1 && errno == ECHILD) {
                // Already reaped by SIGCHLD handler
                ERROR("Child process with pid %d for socket '%s' died unexpectedly.", child_pid, sock->socket_filename);
            } else if (wait_result != 0){
                ERROR("Child process with pid %d for socket '%s' ended with weird error (status: %d).", child_pid, sock->socket_filename, status);
            }

            return_data = send_and_receive_over_socket_wait(sock->socket_filename, ping_packet, ping_timeout, ping_timeout, &errmsg);
            if(errmsg != NULL || return_data == NULL){
                if(attempt == MAX_RETRIES){
                    free(ping_packet);
                    ERROR("Failed to ping '%s':\n%s", sock->socket_filename, errmsg);
                }
                free(errmsg);
                errmsg = NULL;
                free(return_data);
                return_data = NULL;

                // Sleep using exponential backoff
                struct timespec sleep_time = {
                    .tv_sec = (time_t)retry_time,
                    .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9)
                };
                nanosleep(&sleep_time, NULL);

                retry_time *= RETRY_MULTIPLIER;

                ping_timeout = 2 * ping_timeout;
                continue;
            }
            free(return_data);
            break;
        }
        free(ping_packet);
    }
}

// Check for crashed pools (pids[i] == -1) and restart them.
// Called from the daemon event loop on every poll cycle.
void check_and_restart_pools(morloc_socket_t* sockets, size_t n_pools) {
    char* errmsg = NULL;

    for (size_t i = 0; i < n_pools; i++) {
        if (pids[i] != -1) continue;

        fprintf(stderr, "daemon: pool %zu died, restarting...\n", i);

        pid_t new_pid = start_language_server(&sockets[i], &errmsg);
        if (errmsg) {
            fprintf(stderr, "daemon: failed to restart pool %zu: %s\n", i, errmsg);
            free(errmsg);
            errmsg = NULL;
            continue;
        }

        pids[i] = new_pid;
        pgids[i] = new_pid;
        sockets[i].pid = new_pid;

        // Ping with exponential backoff to wait for pool to be ready
        uint8_t* ping_packet = make_ping_packet();
        double retry_time = INITIAL_RETRY_DELAY;
        int ping_timeout = INITIAL_PING_TIMEOUT_MICROSECONDS;
        bool started = false;

        for (int attempt = 0; attempt <= MAX_RETRIES; attempt++) {
            if (pids[i] == -1) {
                fprintf(stderr, "daemon: restarted pool %zu died immediately\n", i);
                break;
            }

            uint8_t* return_data = send_and_receive_over_socket_wait(
                sockets[i].socket_filename, ping_packet,
                ping_timeout, ping_timeout, &errmsg);

            if (errmsg == NULL && return_data != NULL) {
                free(return_data);
                started = true;
                break;
            }
            free(errmsg);
            errmsg = NULL;
            free(return_data);

            struct timespec sleep_time = {
                .tv_sec = (time_t)retry_time,
                .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9)
            };
            nanosleep(&sleep_time, NULL);
            retry_time *= RETRY_MULTIPLIER;
            ping_timeout = 2 * ping_timeout;
        }
        free(ping_packet);

        if (started) {
            fprintf(stderr, "daemon: pool %zu restarted (pid %d)\n", i, (int)new_pid);
        } else {
            fprintf(stderr, "daemon: pool %zu failed to restart\n", i);
        }
    }
}

// Check if a pool at given index is alive
bool pool_is_alive(size_t pool_index) {
    if (pool_index >= MAX_DAEMONS) return false;
    pid_t pid = pids[pool_index];
    if (pid <= 0) return false;
    return (kill(pid, 0) == 0);
}

void run_command(
    uint32_t mid,
    argument_t** args,
    char** arg_schema_strs,
    const char* return_schema_str,
    morloc_socket_t root_socket,
    config_t config
){
    char* errmsg = NULL;

    Schema* return_schema = parse_schema(return_schema_str, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse return schema");
    }

    uint8_t* call_packet = make_call_packet_from_cli(NULL, mid, args, arg_schema_strs, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse arguments:\n%s", errmsg);
    }

    // free arguments
    for(size_t i = 0; args[i] != NULL; i++){
        free_argument_t(args[i]);
    }
    free(args);

    uint8_t* result_packet = send_and_receive_over_socket(root_socket.socket_filename, call_packet, &errmsg);
    if(errmsg != NULL){
        ERROR("Daemon is unresponsive:\n%s", errmsg);
    }

    print_return(result_packet, return_schema, config);

    // cleanup (currently unreachable since print_return calls clean_exit)
    free_schema(return_schema);
    free(call_packet);
    free(result_packet);
}

void run_pure_command(
    morloc_expression_t* expr,
    argument_t** args,
    char** arg_schema_strs,
    const char* return_schema_str,
    config_t config
){
    char* errmsg = NULL;

    size_t nargs = 0;
    for(; args[nargs] != NULL; nargs++){
        if(arg_schema_strs == NULL){
          ERROR("Too many arguments provided");
        }
    }
    if(arg_schema_strs[nargs] != NULL){
        ERROR("Too few arguments provided");
    }

    Schema** arg_schemas = (Schema**)calloc(nargs, sizeof(Schema*));
    uint8_t** arg_packets = (uint8_t**)calloc(nargs, sizeof(uint8_t*));
    uint8_t** arg_voidstars = (uint8_t**)calloc(nargs, sizeof(uint8_t*));

    for(size_t i = 0; i < nargs; i++){
      arg_schemas[i] = parse_schema(arg_schema_strs[i], &errmsg);
      if(errmsg != NULL){
          ERROR("Failed to parse arg schema: %s", errmsg);
      }

      arg_packets[i] = parse_cli_data_argument(NULL, args[i], arg_schemas[i], &errmsg);

      if(errmsg != NULL){
          ERROR("Failed read argument: %s", errmsg);
      }

      arg_voidstars[i] = get_morloc_data_packet_value(arg_packets[i], arg_schemas[i], &errmsg);
      if(errmsg != NULL){
          ERROR("Failed to read arg packet: %s", errmsg);
      }

    }

    Schema* return_schema = parse_schema(return_schema_str, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse return schema");
    }

    absptr_t result_abs = morloc_eval(expr, return_schema, arg_voidstars, arg_schemas, nargs, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to evaluate expression:\n%s", errmsg)
    }

    relptr_t result_rel = abs2rel(result_abs, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to convert absolute pointer to relative:\n%s", errmsg)
    }

    uint8_t* result_packet = make_standard_data_packet(result_rel, return_schema);

    print_return(result_packet, return_schema, config);

    // cleanup (currently unreachable since print_return calls clean_exit)
    for(size_t i = 0; i < nargs; i++){
        free_schema(arg_schemas[i]);
        free(arg_packets[i]);
    }
    free(arg_schemas);
    free(arg_packets);
    free(arg_voidstars);
    free_schema(return_schema);
    free(result_packet);
}


// Run a call packet on a remote worker node
void run_call_packet(config_t config){

    char* errmsg = NULL;
    char* write_errmsg = NULL;
    size_t call_packet_size = 0;
    uint8_t* result_packet = NULL;
    uint8_t* cache_result_packet = NULL;
    char* run_errmsg = NULL;
    char* schema_str = NULL;
    Schema* schema = NULL;

    uint8_t* call_packet = read_binary_file(config.packet_path, &call_packet_size, &errmsg);

    if(errmsg != NULL || call_packet == NULL){
        ERROR("Failed to open call packet file '%s':\n%s", config.packet_path, errmsg)
    }

    // make the local socket filename relative to the current temporary directry
    char socket_path[MAX_FILENAME_SIZE] = { '\0' };
    snprintf(socket_path, sizeof(socket_path), "%s/%s", tmpdir, config.socket_base);

    // send request to the language daemon and wait for a response
    result_packet = send_and_receive_over_socket(socket_path, call_packet, &errmsg);
    if(errmsg != NULL){
        ERROR("Run failed - bad message: %s", errmsg);
    }
    run_errmsg = get_morloc_data_packet_error_message(result_packet, &errmsg);
    if(run_errmsg != NULL){
        ERROR("Run failed: %s", run_errmsg);
    }
    if(errmsg != NULL){
        ERROR("Run failed - unable to parse: %s", errmsg);
    }

    // parse the schema from the response packet
    schema_str = ERROR_TRY_GOTO(read_schema_from_packet_meta, result_packet);
    schema = ERROR_TRY_GOTO(parse_schema, schema_str);

    uint8_t* mlc = ERROR_TRY_GOTO(get_morloc_data_packet_value, result_packet, schema)
    char* mpk_data = NULL; // MessagePack data point
    size_t mpk_size = 0;

    // translate returned data to MessagePack format
    ERROR_TRY_GOTO(pack_with_schema, (void*)mlc, schema, &mpk_data, &mpk_size);

    // make a filename in cachedir for mpk_data
    char mpk_data_filename[MAX_FILENAME_SIZE];
    snprintf(mpk_data_filename, MAX_FILENAME_SIZE, "%s.mpk", config.output_path);

    // write MessagePack data to cache dir
    ERROR_TRY_GOTO(write_atomic, mpk_data_filename, mpk_data, mpk_size);

    // create packet that wraps the message pack file
    cache_result_packet = make_mpk_data_packet(mpk_data_filename, schema);

    // read the packet size based on packet header size, data length and metadata lengths
    size_t cache_result_packet_size = ERROR_TRY_GOTO(morloc_packet_size, cache_result_packet);

    // Write resulting packet (even if it failed)
    ERROR_TRY_GOTO(write_atomic, config.output_path, cache_result_packet, cache_result_packet_size);

end:
    if(schema != NULL){
        free_schema(schema);
    }
    if(mpk_data != NULL){
        free(mpk_data);
    }
    if(call_packet != NULL){
        free(call_packet);
    }
    if(cache_result_packet != NULL){
        free(cache_result_packet);
    }
    if(result_packet != NULL){
        free(result_packet);
    }
    if(errmsg != NULL){
        free(errmsg);
        clean_exit(1);
    }
    clean_exit(0);
}


// ======================================================================
// Manifest-driven socket setup
// ======================================================================

morloc_socket_t* setup_sockets(
    const manifest_t* manifest,
    const char* tmpdir_path,
    const char* shm_basename
){
    morloc_socket_t* sockets = (morloc_socket_t*)calloc(manifest->n_pools, sizeof(morloc_socket_t));

    for (size_t i = 0; i < manifest->n_pools; i++) {
        manifest_pool_t* pool = &manifest->pools[i];
        sockets[i].lang = strdup(pool->lang);

        // Count exec args
        size_t nexec = 0;
        while (pool->exec[nexec]) nexec++;

        // Build syscmd: exec_args... socket_path tmpdir shm_basename NULL
        sockets[i].syscmd = (char**)calloc(nexec + 4, sizeof(char*));
        for (size_t j = 0; j < nexec; j++) {
            sockets[i].syscmd[j] = strdup(pool->exec[j]);
        }

        char buffer[256];
        snprintf(buffer, sizeof(buffer), "%s/%s", tmpdir_path, pool->socket);
        sockets[i].syscmd[nexec] = strdup(buffer);
        sockets[i].syscmd[nexec + 1] = strdup(tmpdir_path);
        sockets[i].syscmd[nexec + 2] = strdup(shm_basename);
        sockets[i].syscmd[nexec + 3] = NULL;
        sockets[i].socket_filename = strdup(buffer);
    }

    return sockets;
}

// ======================================================================
// Manifest-driven help text
// ======================================================================

void print_mim_usage(void) {
    fprintf(stderr, "Usage: mim <manifest> [OPTION...] COMMAND [ARG...]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "mim is the morloc install manager and dispatcher.\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Arguments:\n");
    fprintf(stderr, "  <manifest>           Path to a .manifest file or wrapper script\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -h, --help           Print this help message\n");
    fprintf(stderr, "  -o, --output-file    Print to this file instead of STDOUT\n");
    fprintf(stderr, "  -f, --output-format  Output format [json|mpk|voidstar]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Daemon mode:\n");
    fprintf(stderr, "  --daemon             Run as a long-lived daemon\n");
    fprintf(stderr, "  --socket <path>      Listen on Unix socket\n");
    fprintf(stderr, "  --port <n>           Listen on TCP port (length-prefixed JSON)\n");
    fprintf(stderr, "  --http-port <n>      Listen on HTTP port\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Router mode:\n");
    fprintf(stderr, "  --router             Run as a multi-program router\n");
    fprintf(stderr, "  --fdb <path>         Path to fdb manifest directory\n");
    clean_exit(0);
}

void print_usage(const manifest_t* manifest) {
    fprintf(stderr, "Usage: mim <manifest> [OPTION...] COMMAND [ARG...]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Nexus Options:\n");
    fprintf(stderr, " -h, --help            Print this help message\n");
    fprintf(stderr, " -o, --output-file     Print to this file instead of STDOUT\n");
    fprintf(stderr, " -f, --output-format   Output format [json|mpk|voidstar]\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Exported commands (call with -h/--help for more info):\n");

    // Find longest command name for alignment
    size_t longest = 0;
    for (size_t i = 0; i < manifest->n_commands; i++) {
        size_t len = strlen(manifest->commands[i].name);
        if (len > longest) longest = len;
    }

    for (size_t i = 0; i < manifest->n_commands; i++) {
        manifest_command_t* cmd = &manifest->commands[i];
        size_t namelen = strlen(cmd->name);
        size_t padding = longest - namelen + 2;

        fprintf(stderr, "  %s", cmd->name);
        if (cmd->desc && cmd->desc[0]) {
            for (size_t p = 0; p < padding; p++) fputc(' ', stderr);
            fprintf(stderr, "%s", cmd->desc[0]);
        }
        fprintf(stderr, "\n");
    }
    clean_exit(0);
}

// Print help for a specific subcommand
void print_command_help(const manifest_command_t* cmd) {
    // Usage line
    fprintf(stderr, "Usage: mim <manifest> %s", cmd->name);
    // Check if there are non-positional args
    bool has_opts = false;
    for (size_t i = 0; i < cmd->n_args; i++) {
        if (cmd->args[i].kind != MARG_POS) { has_opts = true; break; }
    }
    if (has_opts) fprintf(stderr, " [OPTION...]");
    for (size_t i = 0; i < cmd->n_args; i++) {
        if (cmd->args[i].kind == MARG_POS)
            fprintf(stderr, " %s", cmd->args[i].metavar ? cmd->args[i].metavar : "ARG");
    }
    fprintf(stderr, "\n");

    // Description
    if (cmd->desc) {
        for (size_t i = 0; cmd->desc[i]; i++) {
            if (i == 0 && cmd->desc[i][0] == '\0') continue; // skip leading blank
            fprintf(stderr, "%s\n", cmd->desc[i]);
        }
    }

    // Positional arguments
    bool has_pos = false;
    for (size_t i = 0; i < cmd->n_args; i++) {
        if (cmd->args[i].kind == MARG_POS) {
            if (!has_pos) { fprintf(stderr, "\nPositional arguments:\n"); has_pos = true; }
            manifest_arg_t* a = &cmd->args[i];
            fprintf(stderr, "  %s", a->metavar ? a->metavar : "ARG");
            if (a->desc && a->desc[0]) fprintf(stderr, "  %s", a->desc[0]);
            fprintf(stderr, "\n");
            if (a->type_desc) fprintf(stderr, "      type: %s\n", a->type_desc);
        }
    }

    // Optional arguments
    bool has_opt = false;
    for (size_t i = 0; i < cmd->n_args; i++) {
        manifest_arg_t* a = &cmd->args[i];
        if (a->kind == MARG_OPT) {
            if (!has_opt) { fprintf(stderr, "\nOptional arguments:\n"); has_opt = true; }
            fprintf(stderr, "    ");
            if (a->short_opt && a->long_opt) fprintf(stderr, "-%c, --%s %s", a->short_opt, a->long_opt, a->metavar);
            else if (a->short_opt) fprintf(stderr, "-%c %s", a->short_opt, a->metavar);
            else if (a->long_opt) fprintf(stderr, "--%s %s", a->long_opt, a->metavar);
            fprintf(stderr, "\n");
            if (a->desc) for (size_t d = 0; a->desc[d]; d++) fprintf(stderr, "        %s\n", a->desc[d]);
            if (a->type_desc) fprintf(stderr, "        type: %s\n", a->type_desc);
            if (a->default_val) fprintf(stderr, "        default: %s\n", a->default_val);
        } else if (a->kind == MARG_FLAG) {
            if (!has_opt) { fprintf(stderr, "\nOptional arguments:\n"); has_opt = true; }
            fprintf(stderr, "    ");
            if (a->short_opt && a->long_opt) fprintf(stderr, "-%c, --%s", a->short_opt, a->long_opt);
            else if (a->short_opt) fprintf(stderr, "-%c", a->short_opt);
            else if (a->long_opt) fprintf(stderr, "--%s", a->long_opt);
            fprintf(stderr, "\n");
            if (a->long_rev) fprintf(stderr, "    --%s\n", a->long_rev);
            if (a->desc) for (size_t d = 0; a->desc[d]; d++) fprintf(stderr, "        %s\n", a->desc[d]);
            if (a->default_val) fprintf(stderr, "        default: %s\n", a->default_val);
        }
    }

    // Group arguments
    for (size_t i = 0; i < cmd->n_args; i++) {
        manifest_arg_t* a = &cmd->args[i];
        if (a->kind == MARG_GRP) {
            fprintf(stderr, "\nGroup arguments:\n");
            fprintf(stderr, "  %s", a->metavar);
            if (a->desc && a->desc[0]) fprintf(stderr, ": %s", a->desc[0]);
            fprintf(stderr, "\n");
            if (a->grp_long) {
                fprintf(stderr, "    ");
                if (a->grp_short) fprintf(stderr, "-%c, ", a->grp_short);
                fprintf(stderr, "--%s %s\n", a->grp_long, a->metavar);
            }
            for (size_t e = 0; e < a->n_entries; e++) {
                manifest_arg_t* ea = a->entries[e].arg;
                fprintf(stderr, "    ");
                if (ea->short_opt && ea->long_opt) {
                    fprintf(stderr, "-%c, --%s", ea->short_opt, ea->long_opt);
                    if (ea->kind == MARG_OPT && ea->metavar) fprintf(stderr, " %s", ea->metavar);
                } else if (ea->long_opt) {
                    fprintf(stderr, "--%s", ea->long_opt);
                    if (ea->kind == MARG_OPT && ea->metavar) fprintf(stderr, " %s", ea->metavar);
                }
                fprintf(stderr, "\n");
                if (ea->desc) for (size_t d = 0; ea->desc[d]; d++) fprintf(stderr, "        %s\n", ea->desc[d]);
            }
        }
    }

    // Return type
    fprintf(stderr, "\nReturn: %s\n", cmd->return_type);
    if (cmd->return_desc) {
        for (size_t i = 0; cmd->return_desc[i]; i++)
            fprintf(stderr, "  %s\n", cmd->return_desc[i]);
    }
    clean_exit(0);
}

// ======================================================================
// Mapping structure for data-driven getopt
// ======================================================================

typedef struct {
    int getopt_val;       // character or long-only int
    size_t arg_idx;       // index into cmd->args
    size_t entry_idx;     // for grp entries; SIZE_MAX for direct opt/flag
    bool is_flag;         // if true, set flag_val instead of optarg
    const char* flag_val; // value to assign for flags
    bool is_grp_opt;      // if true, this is the group-level option
} opt_mapping_t;

// ======================================================================
// Data-driven command dispatch
// ======================================================================

void dispatch_command(
    int argc,
    char* argv[],
    const char* shm_basename,
    config_t config,
    manifest_t* manifest,
    manifest_command_t* cmd,
    morloc_socket_t* sockets
){
    // ---- Phase 1: Build getopt options and slot mapping ----

    struct option long_options[MAX_OPTIONS];
    opt_mapping_t mappings[MAX_OPTIONS];
    char short_opts_buf[MAX_OPTIONS * 3]; // worst case: each short + ':'
    size_t n_long = 0;
    size_t n_map = 0;
    int next_long_val = 257; // 256 reserved for OPT_HELP_VERBOSE
    const int OPT_HELP_VERBOSE = 256;

    // Pre-allocate user-value slots: one per opt/flag/grp_opt/grp_entry
    // We'll index by mapping index
    char* usr_vals[MAX_OPTIONS];
    memset(usr_vals, 0, sizeof(usr_vals));

    // Start building short options: 'h' for help
    short_opts_buf[0] = 'h';
    size_t short_idx = 1;

    // Add --help long option
    long_options[n_long++] = (struct option){"help", no_argument, 0, OPT_HELP_VERBOSE};

    // Helper macro: add a mapping
    #define ADD_MAP(gv, ai, ei, fl, fv, go) do { \
        mappings[n_map].getopt_val = gv; \
        mappings[n_map].arg_idx = ai; \
        mappings[n_map].entry_idx = ei; \
        mappings[n_map].is_flag = fl; \
        mappings[n_map].flag_val = fv; \
        mappings[n_map].is_grp_opt = go; \
        n_map++; \
    } while(0)

    // Helper: register a single opt/flag arg
    #define REG_OPT_FLAG(a, arg_i, ent_i) do { \
        int gval = 0; \
        bool is_flg = (a)->kind == MARG_FLAG; \
        const char* fwd_val = NULL; \
        const char* rev_val = NULL; \
        if (is_flg && (a)->default_val) { \
            fwd_val = (strcmp((a)->default_val, "true") == 0) ? "false" : "true"; \
            rev_val = (strcmp((a)->default_val, "true") == 0) ? "true" : "false"; \
        } \
        if ((a)->short_opt) { \
            gval = (a)->short_opt; \
            short_opts_buf[short_idx++] = (a)->short_opt; \
            if (!is_flg) short_opts_buf[short_idx++] = ':'; \
        } else if ((a)->long_opt) { \
            gval = next_long_val++; \
        } \
        if ((a)->long_opt) { \
            long_options[n_long++] = (struct option){ \
                (a)->long_opt, \
                is_flg ? no_argument : required_argument, \
                0, \
                (a)->short_opt ? (int)(a)->short_opt : gval \
            }; \
            if (!(a)->short_opt) gval = gval; /* already set */ \
        } \
        ADD_MAP(gval, arg_i, ent_i, is_flg, fwd_val, false); \
        if (is_flg && (a)->long_rev) { \
            int rev_gval = next_long_val++; \
            long_options[n_long++] = (struct option){(a)->long_rev, no_argument, 0, rev_gval}; \
            ADD_MAP(rev_gval, arg_i, ent_i, true, rev_val, false); \
        } \
    } while(0)

    for (size_t i = 0; i < cmd->n_args; i++) {
        manifest_arg_t* a = &cmd->args[i];
        if (a->kind == MARG_OPT || a->kind == MARG_FLAG) {
            REG_OPT_FLAG(a, i, SIZE_MAX);
        } else if (a->kind == MARG_GRP) {
            // Group-level option
            if (a->grp_long) {
                int gval = 0;
                if (a->grp_short) {
                    gval = a->grp_short;
                    short_opts_buf[short_idx++] = a->grp_short;
                    short_opts_buf[short_idx++] = ':';
                } else {
                    gval = next_long_val++;
                }
                long_options[n_long++] = (struct option){
                    a->grp_long,
                    required_argument,
                    0,
                    a->grp_short ? (int)a->grp_short : gval
                };
                ADD_MAP(gval, i, SIZE_MAX, false, NULL, true);
            }
            // Group entries
            for (size_t e = 0; e < a->n_entries; e++) {
                REG_OPT_FLAG(a->entries[e].arg, i, e);
            }
        }
    }

    #undef ADD_MAP
    #undef REG_OPT_FLAG

    short_opts_buf[short_idx] = '\0';
    // Terminate long options
    long_options[n_long] = (struct option){0, 0, 0, 0};

    // ---- Phase 2: Run getopt_long ----

    // Prepend '+' to short options to stop at first non-option (POSIX mode)
    char posix_short[MAX_OPTIONS * 3 + 1];
    posix_short[0] = '+';
    memcpy(posix_short + 1, short_opts_buf, short_idx + 1);

    int opt;
    while ((opt = getopt_long(argc, argv, posix_short, long_options, NULL)) != -1) {
        if (opt == 'h') {
            print_command_help(cmd);
        }
        if (opt == OPT_HELP_VERBOSE) {
            print_command_help(cmd);
        }
        // Find matching mapping
        bool found = false;
        for (size_t m = 0; m < n_map; m++) {
            if (mappings[m].getopt_val == opt) {
                if (mappings[m].is_flag) {
                    usr_vals[m] = (char*)mappings[m].flag_val;
                } else {
                    usr_vals[m] = optarg;
                }
                found = true;
                break;
            }
        }
        if (!found) {
            fprintf(stderr, "Unknown option\n");
            clean_exit(EXIT_FAILURE);
        }
    }

    // ---- Phase 3: Start daemons (remote commands only) ----

    if (!cmd->is_pure) {
        morloc_socket_t** needed = (morloc_socket_t**)calloc(cmd->n_needed_pools + 1, sizeof(morloc_socket_t*));
        for (size_t i = 0; i < cmd->n_needed_pools; i++) {
            needed[i] = &sockets[cmd->needed_pools[i]];
        }
        needed[cmd->n_needed_pools] = NULL;
        start_daemons(needed);
        free(needed);
    }

    // ---- Phase 4: Build argument_t** array ----

    argument_t** args = (argument_t**)calloc(cmd->n_args + 1, sizeof(argument_t*));
    size_t map_cursor = 0; // cursor into mappings array

    for (size_t i = 0; i < cmd->n_args; i++) {
        manifest_arg_t* a = &cmd->args[i];

        if (a->kind == MARG_POS) {
            if (argv[optind] == NULL) {
                fprintf(stderr, "Error: too few positional arguments\n");
                clean_exit(EXIT_FAILURE);
            }
            char* val;
            if (a->quoted) {
                val = quoted(argv[optind]);
            } else {
                val = strdup(argv[optind]);
            }
            args[i] = initialize_positional(val);
            free(val);
            optind++;
        } else if (a->kind == MARG_OPT) {
            // Find the mapping for this arg
            char* usr = NULL;
            char* def = a->default_val;
            for (size_t m = map_cursor; m < n_map; m++) {
                if (mappings[m].arg_idx == i && mappings[m].entry_idx == SIZE_MAX && !mappings[m].is_grp_opt) {
                    usr = usr_vals[m];
                    map_cursor = m + 1;
                    break;
                }
            }
            if (usr == NULL) {
                if (def == NULL) {
                    args[i] = initialize_positional(NULL);
                } else {
                    args[i] = initialize_positional(strdup(def));
                }
            } else {
                if (a->quoted) {
                    char* q = quoted(usr);
                    args[i] = initialize_positional(q);
                    free(q);
                } else {
                    args[i] = initialize_positional(strdup(usr));
                }
            }
        } else if (a->kind == MARG_FLAG) {
            char* usr = NULL;
            char* def = a->default_val;
            for (size_t m = map_cursor; m < n_map; m++) {
                if (mappings[m].arg_idx == i && mappings[m].entry_idx == SIZE_MAX) {
                    if (usr_vals[m] != NULL) usr = usr_vals[m];
                    // Don't break - also check reverse mapping
                    if (!mappings[m].is_flag || usr != NULL) { map_cursor = m + 1; break; }
                }
            }
            if (usr == NULL) {
                args[i] = initialize_positional(strdup(def ? def : "false"));
            } else {
                args[i] = initialize_positional(strdup(usr));
            }
        } else if (a->kind == MARG_GRP) {
            size_t size = a->n_entries;
            // Find group-level option value
            char* grp_val = NULL;
            for (size_t m = 0; m < n_map; m++) {
                if (mappings[m].arg_idx == i && mappings[m].is_grp_opt && usr_vals[m]) {
                    grp_val = usr_vals[m];
                    break;
                }
            }

            char** fields = (char**)calloc(size, sizeof(char*));
            char** def_fields = (char**)calloc(size, sizeof(char*));

            for (size_t e = 0; e < size; e++) {
                manifest_arg_t* ea = a->entries[e].arg;
                char* entry_usr = NULL;
                char* entry_def = ea->default_val;

                // Find mapping for this entry
                for (size_t m = 0; m < n_map; m++) {
                    if (mappings[m].arg_idx == i && mappings[m].entry_idx == e && usr_vals[m]) {
                        entry_usr = usr_vals[m];
                        break;
                    }
                }

                if (entry_usr != NULL) {
                    if (ea->quoted) {
                        fields[e] = quoted(entry_usr);
                    } else {
                        fields[e] = strdup(entry_usr);
                    }
                }
                if (entry_def != NULL) {
                    def_fields[e] = strdup(entry_def);
                }
            }

            args[i] = initialize_unrolled(size, grp_val, fields, def_fields);

            for (size_t e = 0; e < size; e++) {
                free(fields[e]);
                free(def_fields[e]);
            }
            free(fields);
            free(def_fields);
        }
    }
    args[cmd->n_args] = NULL;

    // Check for extra positional args
    if (argv[optind] != NULL) {
        fprintf(stderr, "Error: too many positional arguments given\n");
        clean_exit(EXIT_FAILURE);
    }

    // ---- Phase 5: Dispatch ----

    if (cmd->is_pure) {
        run_pure_command(cmd->expr, args, cmd->arg_schemas, cmd->return_schema, config);
    } else {
        run_command(cmd->mid, args, cmd->arg_schemas, cmd->return_schema,
                    sockets[cmd->pool_index], config);
    }
}


void dispatch(
    int argc,
    char* argv[],
    const char* shm_basename,
    config_t config,
    manifest_t* manifest,
    morloc_socket_t* sockets
){
    // Handle call-packet mode
    if (config.packet_path != NULL) {
        morloc_socket_t** all = (morloc_socket_t**)calloc(manifest->n_pools + 1, sizeof(morloc_socket_t*));
        for (size_t i = 0; i < manifest->n_pools; i++) all[i] = &sockets[i];
        all[manifest->n_pools] = NULL;
        start_daemons(all);
        free(all);
        run_call_packet(config);
        clean_exit(0);
    }

    char* cmd = argv[optind];
    optind++;

    for (size_t i = 0; i < manifest->n_commands; i++) {
        if (strcmp(cmd, manifest->commands[i].name) == 0) {
            dispatch_command(argc, argv, shm_basename, config,
                             manifest, &manifest->commands[i], sockets);
            return; // dispatch_command calls clean_exit
        }
    }

    fprintf(stderr, "Unrecognized command '%s'\n", cmd);
    clean_exit(1);
}

// Read manifest payload from a file. If the file starts with "#!", scan for
// the "### MANIFEST ###" marker and read the payload after it. Otherwise read
// the entire file as the payload.
char* read_manifest_payload(const char* path, char** errmsg) {
    FILE* f = fopen(path, "r");
    if (!f) {
        *errmsg = strdup("Cannot open manifest file");
        return NULL;
    }

    // Check for shebang
    int c1 = fgetc(f);
    int c2 = fgetc(f);
    if (c1 == '#' && c2 == '!') {
        // Scan for "### MANIFEST ###\n"
        char line[4096];
        // finish reading the shebang line
        if (!fgets(line, sizeof(line), f)) {
            fclose(f);
            *errmsg = strdup("Unexpected EOF in wrapper script");
            return NULL;
        }
        int found = 0;
        while (fgets(line, sizeof(line), f)) {
            if (strncmp(line, "### MANIFEST ###", 16) == 0) {
                found = 1;
                break;
            }
        }
        if (!found) {
            fclose(f);
            *errmsg = strdup("No ### MANIFEST ### marker found in wrapper script");
            return NULL;
        }
        // Read rest of file as payload
        long start = ftell(f);
        fseek(f, 0, SEEK_END);
        long end = ftell(f);
        long size = end - start;
        fseek(f, start, SEEK_SET);
        char* payload = (char*)malloc(size + 1);
        if (!payload) {
            fclose(f);
            *errmsg = strdup("Out of memory reading manifest payload");
            return NULL;
        }
        size_t nread = fread(payload, 1, size, f);
        payload[nread] = '\0';
        fclose(f);
        return payload;
    } else {
        // Regular manifest file - read entire contents
        rewind(f);
        fseek(f, 0, SEEK_END);
        long size = ftell(f);
        rewind(f);
        char* payload = (char*)malloc(size + 1);
        if (!payload) {
            fclose(f);
            *errmsg = strdup("Out of memory reading manifest");
            return NULL;
        }
        size_t nread = fread(payload, 1, size, f);
        payload[nread] = '\0';
        fclose(f);
        return payload;
    }
}

// Validate that all pool executables listed in the manifest exist
void validate_pools(const manifest_t* manifest) {
    for (size_t i = 0; i < manifest->n_pools; i++) {
        manifest_pool_t* pool = &manifest->pools[i];
        if (pool->exec && pool->exec[0]) {
            // For interpreted languages the first arg is the interpreter,
            // the second is the script. For compiled languages, the first
            // arg is the executable itself. Check the last non-NULL arg.
            size_t last = 0;
            while (pool->exec[last + 1]) last++;
            struct stat st;
            if (stat(pool->exec[last], &st) != 0) {
                fprintf(stderr, "Error: Build artifacts missing or stale. "
                    "Pool file '%s' not found. Re-run `morloc make`.\n",
                    pool->exec[last]);
                exit(EXIT_FAILURE);
            }
        }
    }
}

// Long-only option codes
enum {
    OPT_DAEMON    = 300,
    OPT_ROUTER    = 301,
    OPT_SOCKET    = 302,
    OPT_PORT      = 303,
    OPT_HTTP_PORT = 304,
    OPT_FDB       = 305
};

static void parse_nexus_options(int argc, char* argv[], config_t* config) {
    static struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {"call-packet", required_argument, 0, 'c'},
        {"socket-base", required_argument, 0, 's'},
        {"output-file", required_argument, 0, 'o'},
        {"output-form", required_argument, 0, 'f'},
        {"daemon",      no_argument,       0, OPT_DAEMON},
        {"router",      no_argument,       0, OPT_ROUTER},
        {"socket",      required_argument, 0, OPT_SOCKET},
        {"port",        required_argument, 0, OPT_PORT},
        {"http-port",   required_argument, 0, OPT_HTTP_PORT},
        {"fdb",         required_argument, 0, OPT_FDB},
        {NULL, 0, NULL, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "+hc:s:o:f:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'h': config->help_flag = 1; break;
            case 'c': config->packet_path = optarg; break;
            case 's': config->socket_base = optarg; break;
            case 'o': config->output_path = optarg; break;
            case 'f':
                if (strcmp(optarg, "json") == 0) config->output_format = JSON;
                else if (strcmp(optarg, "mpk") == 0) config->output_format = MessagePack;
                else if (strcmp(optarg, "voidstar") == 0) config->output_format = VoidStar;
                else { fprintf(stderr, "Invalid output format: %s\n", optarg); exit(EXIT_FAILURE); }
                break;
            case OPT_DAEMON:    config->daemon_flag = 1; break;
            case OPT_ROUTER:    config->router_flag = 1; break;
            case OPT_SOCKET:    config->unix_socket_path = optarg; break;
            case OPT_PORT:      config->tcp_port = atoi(optarg); break;
            case OPT_HTTP_PORT: config->http_port = atoi(optarg); break;
            case OPT_FDB:       config->fdb_path = optarg; break;
            case '?':
                fprintf(stderr, "Unknown option: %c\n", optopt);
                exit(EXIT_FAILURE);
            case ':':
                fprintf(stderr, "Option %c requires an argument\n", optopt);
                exit(EXIT_FAILURE);
        }
    }
}

int main(int argc, char *argv[]) {

    config_t config = {0};
    config.output_format = JSON;

    // First pass: parse options before the manifest path
    parse_nexus_options(argc, argv, &config);

    // Handle --router mode (no manifest needed)
    if (config.router_flag) {
        char fdb_path_buf[512];
        const char* fdb_path;
        if (config.fdb_path) {
            fdb_path = config.fdb_path;
        } else {
            const char* home = getenv("HOME");
            if (!home) {
                fprintf(stderr, "Error: HOME not set\n");
                exit(EXIT_FAILURE);
            }
            snprintf(fdb_path_buf, sizeof(fdb_path_buf), "%s/.local/share/morloc/fdb", home);
            fdb_path = fdb_path_buf;
        }

        char* errmsg = NULL;
        router_t* router = router_init(fdb_path, &errmsg);
        if (errmsg) {
            fprintf(stderr, "Failed to initialize router: %s\n", errmsg);
            exit(EXIT_FAILURE);
        }

        daemon_config_t dc = {
            .unix_socket_path = config.unix_socket_path,
            .tcp_port = config.tcp_port,
            .http_port = config.http_port
        };
        router_run(&dc, router);
        router_free(router);
        exit(EXIT_SUCCESS);
    }

    // If -h with no manifest argument, show mim's own help
    if (config.help_flag && optind >= argc) {
        print_mim_usage();
    }

    // Require a manifest path as the first positional argument
    if (optind >= argc) {
        print_mim_usage();
    }

    const char* manifest_path = argv[optind];
    optind++;

    // Second pass: parse options that appear after the manifest path
    parse_nexus_options(argc, argv, &config);

    // Read manifest payload (handles both wrapper scripts and plain files)
    char* errmsg = NULL;
    char* payload = read_manifest_payload(manifest_path, &errmsg);
    if (errmsg != NULL) {
        fprintf(stderr, "Failed to load manifest '%s':\n%s\n", manifest_path, errmsg);
        exit(EXIT_FAILURE);
    }

    manifest_t* manifest = parse_manifest(payload, &errmsg);
    free(payload);
    if (errmsg != NULL) {
        fprintf(stderr, "Failed to parse manifest '%s':\n%s\n", manifest_path, errmsg);
        exit(EXIT_FAILURE);
    }

    // chdir to the build directory so relative pool paths resolve correctly
    if (manifest->build_dir) {
        if (chdir(manifest->build_dir) != 0) {
            fprintf(stderr, "Cannot chdir to build_dir '%s': %s\n",
                    manifest->build_dir, strerror(errno));
            exit(EXIT_FAILURE);
        }
    }

    // Validate pool executables exist
    validate_pools(manifest);

    // Handle help flag with manifest loaded (show module help)
    if (config.help_flag) {
        print_usage(manifest);
        exit(EXIT_SUCCESS);
    }

    // set the global temporary directory
    tmpdir = make_tmpdir(&errmsg);

    if(errmsg != NULL){
        ERROR("%s", errmsg);
    }

    uint64_t job_hash = make_job_hash(42);

    size_t shm_initial_size = 0xffff;
    char shm_basename[MAX_FILENAME_SIZE] = { '\0' };
    snprintf(shm_basename, sizeof(shm_basename), "morloc-%" PRIu64, job_hash);

    shm_t* shm = shinit(shm_basename, 0, shm_initial_size, &errmsg);
    if(errmsg != NULL){
        ERROR("%s", errmsg);
    }

    // Become subreaper so orphaned grandchildren (e.g., R pool workers)
    // get reparented to us instead of init, allowing us to reap them
    prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0);

    // Register SIGCHLD handler to reap child processes promptly
    struct sigaction sa_chld;
    sa_chld.sa_handler = sigchld_handler;
    sigemptyset(&sa_chld.sa_mask);
    sa_chld.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    sigaction(SIGCHLD, &sa_chld, NULL);

    // Setup sockets from manifest
    morloc_socket_t* sockets = setup_sockets(manifest, tmpdir, shm_basename);

    // Daemon mode: start all pools and run the daemon event loop
    if (config.daemon_flag) {
        // Start all language pools
        morloc_socket_t** all = (morloc_socket_t**)calloc(manifest->n_pools + 1, sizeof(morloc_socket_t*));
        for (size_t i = 0; i < manifest->n_pools; i++) all[i] = &sockets[i];
        all[manifest->n_pools] = NULL;
        start_daemons(all);
        free(all);

        daemon_config_t dc = {
            .unix_socket_path = config.unix_socket_path,
            .tcp_port = config.tcp_port,
            .http_port = config.http_port,
            .pool_check_fn = check_and_restart_pools,
            .pool_alive_fn = pool_is_alive,
            .n_pools = manifest->n_pools
        };
        daemon_run(&dc, manifest, sockets, manifest->n_pools, shm_basename);
        clean_exit(0);
    }

    // Command logic routing (normal CLI mode)
    if (config.packet_path == NULL) {
        // Require subcommand when not using call packets
        if (optind >= argc) {
            print_usage(manifest);
            clean_exit(EXIT_FAILURE);
        }

        // Validate we don't have conflicting options
        if (config.socket_base) {
            fprintf(stderr, "Error: socket-base can't be used with subcommands\n");
            clean_exit(EXIT_FAILURE);
        }

        dispatch(argc, argv, shm_basename, config, manifest, sockets);
    } else {
        // Validate no positional arguments when using call packet
        if (optind < argc) {
            fprintf(stderr, "Error: Positional arguments not allowed with --call-packet\n");
            clean_exit(EXIT_FAILURE);
        }

        // Validate required fields for packet mode
        if (!config.socket_base) {
            fprintf(stderr, "Error: socket-base required for call packet mode\n");
            clean_exit(EXIT_FAILURE);
        }

        dispatch(argc, argv, shm_basename, config, manifest, sockets);
    }

    // unreachable
    clean_exit(EXIT_SUCCESS);
}
