#include "morloc.h"

#include <errno.h>
#include <getopt.h>
#include <inttypes.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
} config_t;

// global pid list of language daemons
// a pid of 0 means unused, -1 means already reaped
pid_t pids[MAX_DAEMONS] = { 0 };

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

    // Phase 1: Send SIGTERM to all live pools
    for (size_t i = 0; i < MAX_DAEMONS; i++) {
        if (pids[i] > 0) {
            kill(pids[i], SIGTERM);
        }
    }

    // Phase 2: Wait for pools to exit (up to 500ms per pool, then SIGKILL)
    for (size_t i = 0; i < MAX_DAEMONS; i++) {
        if (pids[i] <= 0) continue; // unused or already reaped

        // Poll with non-blocking waitpid, sleeping 10ms between checks
        int reaped = 0;
        for (int attempt = 0; attempt < 50; attempt++) {
            int status;
            pid_t result = waitpid(pids[i], &status, WNOHANG);
            if (result == pids[i] || result == -1) {
                pids[i] = -1;
                reaped = 1;
                break;
            }
            struct timespec ts = { .tv_sec = 0, .tv_nsec = 10000000 }; // 10ms
            nanosleep(&ts, NULL);
        }

        // Escalate to SIGKILL if still alive
        if (!reaped) {
            kill(pids[i], SIGKILL);
            waitpid(pids[i], NULL, 0); // blocking reap after SIGKILL
            pids[i] = -1;
        }
    }

    // Phase 3: Clean up resources (all pools are dead now)
    if (tmpdir != NULL) {
        delete_directory(tmpdir);
        free(tmpdir);
        tmpdir = NULL;
    }

    char* errmsg = NULL;
    shclose(&errmsg);
    if(errmsg != NULL){
        fprintf(stderr, "%s", errmsg);
        exit(1);
    }

    exit(exit_code);
}

typedef struct morloc_socket_s{
    char* lang;
    char** syscmd;
    char* socket_filename;
    int pid; // language server pid
} morloc_socket_t;


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
        execvp(socket->syscmd[0], socket->syscmd);
        RAISE("execvp failed: %s", strerror(errno)) // Only reached if exec fails
    } else if (pid > 0) { // Parent process
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
                    ERROR("Failed to ping '%s':\n%s", sock->socket_filename, errmsg);
                }

                // Sleep using exponential backoff
                struct timespec sleep_time = {
                    .tv_sec = (time_t)retry_time,
                    .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9)
                };
                nanosleep(&sleep_time, NULL);

                retry_time *= RETRY_MULTIPLIER;

                // Increase the ping timeout
                //
                // An infinite timeout, of course, would hang on unresponsive
                // daemons. But if the timeout is too short, the daemon may not have
                // time to respond. And response time decreases when the system is
                // under heavy load, which in the past caused non-deterministic
                // freezes.
                ping_timeout = 2 * ping_timeout;
                continue;
            }
            break;
        }
    }
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
    // args are the remaining char pointers from argv, these are guaranteed to
    // end with a NULL pointer
    for(; args[nargs] != NULL; nargs++){
        // assert that there are an appropriate number of schemas
        // mismatch implies an input mistake on the users side
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
}


// Run a call packet on a remote worker node
void run_call_packet(config_t config){

    char* errmsg = NULL;
    char* write_errmsg = NULL;
    size_t call_packet_size = 0;
    uint8_t* result_packet = NULL;
    uint8_t* cache_result_packet = NULL;
    char* run_errmsg = NULL;

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
    char* schema_str = ERROR_TRY_GOTO(read_schema_from_packet_meta, result_packet);
    Schema* schema = ERROR_TRY_GOTO(parse_schema, schema_str);

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


void usage(){
    // AUTO general usage statement
    // <<<BREAK>>>
    // AUTO usage
    clean_exit(0);
}

// AUTO subcommand dispatchers
// <<<BREAK>>>
// AUTO subcommand dispatchers

void dispatch(
    int argc,
    char* argv[],
    const char* shm_basename,
    config_t config
){

    char* cmd = argv[optind];
    optind++;

// AUTO dispatch
// <<<BREAK>>>
// AUTO dispatch

    clean_exit(0);
}

int main(int argc, char *argv[]) {

    config_t config = { 0, NULL, NULL, NULL, JSON };

    struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {"call-packet", required_argument, 0, 'c'},
        {"socket-base", required_argument, 0, 's'},
        {"output-file", required_argument, 0, 'o'}, // where to write nexus final output
        {"output-form", required_argument, 0, 'f'},
        {NULL, 0, NULL, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "+hc:s:o:f:", long_options, NULL)) != -1) {
        switch (opt) {
            case 'h':
                config.help_flag = 1;
                break;

            case 'c':
                config.packet_path = optarg;
                break;

            case 's':
                config.socket_base = optarg;
                break;

            case 'o':
                config.output_path = optarg;
                break;

            case 'f':
                if (strcmp(optarg, "json") == 0) {
                    config.output_format = JSON;
                } else if (strcmp(optarg, "mpk") == 0) {
                    config.output_format = MessagePack;
                } else if (strcmp(optarg, "voidstar") == 0) {
                    config.output_format = VoidStar;
                } else {
                    fprintf(stderr, "Invalid output format: %s\n", optarg);
                    exit(EXIT_FAILURE);
                }
                break;

            case '?':
                fprintf(stderr, "Unknown option: %c\n", optopt);
                exit(EXIT_FAILURE);

            case ':':
                fprintf(stderr, "Option %c requires an argument\n", optopt);
                exit(EXIT_FAILURE);
        }
    }

    // Handle help flag immediately
    if (config.help_flag) {
        usage();
        exit(EXIT_SUCCESS);
    }


    char* errmsg = NULL;

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

    // Register SIGCHLD handler to reap child processes promptly
    struct sigaction sa_chld;
    sa_chld.sa_handler = sigchld_handler;
    sigemptyset(&sa_chld.sa_mask);
    sa_chld.sa_flags = SA_RESTART | SA_NOCLDSTOP;
    sigaction(SIGCHLD, &sa_chld, NULL);

    // Command logic routing
    if (config.packet_path == NULL) {
        // Require subcommand when not using call packets
        if (optind >= argc) {
            usage();
            clean_exit(EXIT_FAILURE);
        }

        // Validate we don't have conflicting options
        if (config.socket_base) {
            fprintf(stderr, "Error: socket-base can't be used with subcommands\n");
            clean_exit(EXIT_FAILURE);
        }

        dispatch(argc, argv, shm_basename, config);
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

        dispatch(argc, argv, shm_basename, config);
    }

    // unrechable
    clean_exit(EXIT_SUCCESS);
}
