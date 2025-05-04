#include "morloc.h"

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
    fprintf(stderr, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    clean_exit(1);

#define ERROR_WITH(end, msg, ...) \
    fprintf(stderr, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    end; \
    clean_exit(1);

#define ERROR_TRY_GOTO(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &errmsg); \
    if(errmsg != NULL){ \
        fprintf(stderr, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, errmsg); \
        goto end; \
    }

typedef enum {
    JSON,
    MessagePack,
    Packet
} format_enum;

typedef struct config_s {
    int help_flag;
    char* packet_path;
    char* socket_base;
    char* output_path;
    format_enum output_format;
} config_t;

// global pid list of language daemons
int pids[MAX_DAEMONS] = { 0 }; 

// global temporary file
char* tmpdir = NULL;


void clean_exit(int exit_code){
    // Kill all daemon processes
    for (size_t i = 0; i < MAX_DAEMONS; i++) {
        if (pids[i] > 0) { // Ensure pid is valid
            if (kill(pids[i], SIGTERM) == -1) { // Send SIGTERM to gracefully terminate
                perror("Failed to kill process");
            }
        }
    }

    // Delete the temporary directory and its contents
    if (tmpdir != NULL) {
        delete_directory(tmpdir);
        tmpdir = NULL; // Clear the pointer
    }

    char* errmsg = NULL;
    shclose(&errmsg);
    if(errmsg != NULL){
        fprintf(stderr, errmsg);
        exit(1);
    }

    exit(exit_code); // Exit with the provided code
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

    return XXH64(buffer, strlen(buffer), seed);
}


char* make_tmpdir(ERRMSG) {
    PTR_RETURN_SETUP(char)

    // set to static so that the value can be used after exit
    static char template[] = "/tmp/morloc.XXXXXX";

    // note that this is a pointer to template after mkdtemp mutates it
    // so it is still on the stack and needn't be freed
    char* temp_dir = mkdtemp(template);
    RAISE_IF(temp_dir == NULL, "Failed to create temporary directory")

    return temp_dir;
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


void print_return(uint8_t* packet, Schema* schema){
    char* child_errmsg = NULL;

    char* packet_error = get_morloc_data_packet_error_message(packet, &child_errmsg);
    if(packet_error != NULL){
        ERROR("Run failed\n%s\n", packet_error)
    }
    if(child_errmsg != NULL){
        ERROR("Internal error\n%s\n", packet_error)
    }

    uint8_t* packet_value = get_morloc_data_packet_value(packet, schema, &child_errmsg);
    if(child_errmsg != NULL){
        ERROR("%s", child_errmsg);
    }

    // print result
    print_voidstar(packet_value, schema, &child_errmsg);
    if(child_errmsg == NULL){
        clean_exit(0);
    } else {
        ERROR("Failed to print return packet\n%s\n", child_errmsg);
    }
}


void start_daemons(morloc_socket_t** all_sockets){
    int pid = 0;
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
    for(morloc_socket_t** socket_ptr = all_sockets; *socket_ptr != NULL; socket_ptr++){
        uint8_t* ping_packet = make_ping_packet();
        double retry_time = INITIAL_RETRY_DELAY;
        int ping_timeout = INITIAL_PING_TIMEOUT_MICROSECONDS;
        uint8_t* return_data;
        for(int attempt = 0; attempt <= MAX_RETRIES; attempt++){
            return_data = send_and_receive_over_socket_wait((*socket_ptr)->socket_filename, ping_packet, ping_timeout, ping_timeout, &errmsg);
            if(errmsg != NULL || return_data == NULL){
                if(attempt == MAX_RETRIES){
                    ERROR("Failed to ping '%s':\n%s\n", (*socket_ptr)->socket_filename, errmsg);
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
    char** args,
    const char** arg_schema_strs,
    const char* return_schema_str,
    morloc_socket_t root_socket
){
    char* errmsg = NULL;

    Schema* return_schema = parse_schema(&return_schema_str, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse return schema");
    }

    uint8_t* call_packet = make_call_packet_from_cli(mid, args, arg_schema_strs, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse arguments:\n%s", errmsg);
    }

    uint8_t* result_packet = send_and_receive_over_socket(root_socket.socket_filename, call_packet, &errmsg);
    if(errmsg != NULL){
        ERROR("Daemon is unresponsive:\n%s\n", errmsg);
    }

    print_return(result_packet, return_schema);
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
        ERROR("Failed to open call packet file '%s':\n%s\n", config.packet_path, errmsg)
    }

    // make the local socket filename relative to the current temporary directry
    char socket_path[MAX_FILENAME_SIZE] = { '\0' };
    snprintf(socket_path, sizeof(socket_path), "%s/%s", tmpdir, config.socket_base);

    // send request to the language daemon and wait for a response
    result_packet = send_and_receive_over_socket(socket_path, call_packet, &errmsg);
    if(errmsg != NULL){
        ERROR("Run failed - bad message: %s\n", errmsg);
    }
    run_errmsg = get_morloc_data_packet_error_message(result_packet, &errmsg);
    if(run_errmsg != NULL){
        ERROR("Run failed: %s\n", run_errmsg);
    }
    if(errmsg != NULL){
        ERROR("Run failed - unable to parse: %s\n", errmsg);
    }

    // parse the schema from the response packet
    char* schema_str = ERROR_TRY_GOTO(read_schema_from_packet_meta, result_packet);
    Schema* schema = ERROR_TRY_GOTO(parse_schema, (const char**)&schema_str);

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


void dispatch(
    const char* cmd,
    char** args,
    const char* shm_basename,
    config_t config
){

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
        {"output-file", required_argument, 0, 'o'},
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
                } else if (strcmp(optarg, "packet") == 0) {
                    config.output_format = Packet;
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

    // Command logic routing
    if (config.packet_path == NULL) {
        // Require subcommand when not using call packets
        if (optind >= argc) {
            usage();
            clean_exit(EXIT_FAILURE);
        }

        // Extract subcommand and arguments
        const char* subcommand = argv[optind];
        char** command_arguments = &argv[optind + 1];

        // Validate we don't have conflicting options
        if (config.socket_base) {
            fprintf(stderr, "Error: socket-base can't be used with subcommands\n");
            clean_exit(EXIT_FAILURE);
        }

        dispatch(subcommand, command_arguments, shm_basename, config);
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

        dispatch(NULL, NULL, shm_basename, config);
    }

    // unrechable
    clean_exit(EXIT_SUCCESS);
}
