#include "morloc.h"

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

#define INITIAL_RETRY_DELAY 0.001
#define RETRY_MULTIPLIER 1.25
#define MAX_RETRIES 45

#define MAX_DAEMONS 32

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

#define ERROR(msg, ...) \
    fprintf(stderr, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    clean_exit(1);


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


bool is_help(char* arg1){
    return (
        strcmp(arg1, "-h") == 0 ||
        strcmp(arg1, "--help") == 0 ||
        strcmp(arg1, "-?") == 0 ||
        strcmp(arg1, "?") == 0
    );
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

    bool failed = get_morloc_data_packet_error_message(packet, &child_errmsg);
    // print error due to malformed packet
    if(failed){
        if(child_errmsg == NULL){
            ERROR("Failed to read returned data packet\n");
        } else {
            ERROR("Failed to read returned data packet:\n%s\n", child_errmsg);
        }
    }

    // print error raised from inside morloc
    if(child_errmsg != NULL){
        ERROR("%s", child_errmsg);
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
        ERROR("Failed to print return packet:\n%s\n", child_errmsg);
    }
}


void run_command(
    uint32_t mid,
    char** args,
    const char** arg_schema_strs,
    const char* return_schema_str,
    morloc_socket_t root_socket,
    morloc_socket_t** all_sockets // not const, these are modified to add pid
){
    int pid = 0;
    char* errmsg = NULL;

    Schema* return_schema = parse_schema(&return_schema_str, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse return schema\n");
    }

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
    uint8_t* ping_packet = make_ping_packet();
    bool all_pass;
    uint8_t* return_data;
    double retry_time = INITIAL_RETRY_DELAY;
    int attempts = 0;
    while(!all_pass){
        all_pass = true;
        for(morloc_socket_t** socket_ptr = all_sockets; *socket_ptr != NULL; socket_ptr++){
            return_data = send_and_receive_over_socket((*socket_ptr)->socket_filename, ping_packet, &errmsg);
            if(errmsg != NULL || return_data == NULL){
                all_pass = false;
                fprintf(stderr, "Failed to ping '%s', waiting %fs\n", (*socket_ptr)->socket_filename, retry_time);
                if(errmsg){
                    fprintf(stderr, " %s", errmsg);
                }
                break;
            }
        }
        if(!all_pass){
            // Sleep using exponential backoff
            struct timespec sleep_time = {
                .tv_sec = (time_t)retry_time,
                .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9)
            };
            nanosleep(&sleep_time, NULL);
            
            retry_time *= RETRY_MULTIPLIER;
            attempts++;
            
            if(attempts > MAX_RETRIES){
                ERROR("Timed out after %d attempts while waiting for language servers to start\n", attempts);
            }
        }
    }

    uint8_t* call_packet = make_call_packet_from_cli(mid, args, arg_schema_strs, &errmsg);
    if(errmsg != NULL){
        ERROR("Failed to parse arguments\n%s", errmsg);
    }

    uint8_t* result_packet = send_and_receive_over_socket(root_socket.socket_filename, call_packet, &errmsg);
    if(errmsg != NULL){
        ERROR("Daemon is unresponsive: %s\n", errmsg);
    }

    print_return(result_packet, return_schema);
}


void usage(){
    // <<<BREAK>>>
    clean_exit(0);
}


int dispatch(
    const char* cmd,
    char** args,
    const char* shm_basename,
    size_t shm_initial_size
){

// AUTO usage and dispatch
// <<<BREAK>>>
// AUTO usage and dispatch

    clean_exit(0);
}


int main(int argc, char* argv[]){
    char* errmsg = NULL;

    if(argc == 1 || (argv[1] != NULL && is_help(argv[1]))){
        usage();
        return 1;
    }

    uint64_t job_hash = make_job_hash(42);

    size_t shm_initial_size = 0xffff;
    char shm_basename[MAX_FILENAME_SIZE];
    snprintf(shm_basename, sizeof(shm_basename), "morloc-%" PRIu64, job_hash);

    shm_t* shm = shinit(shm_basename, 0, shm_initial_size, &errmsg);
    if(errmsg != NULL){
        ERROR("%s", errmsg);
    }

    char* cmd = argv[1];

    // set the global temporary directory
    tmpdir = make_tmpdir(&errmsg);

    if(errmsg != NULL){
        ERROR("%s", errmsg);
    }

    return dispatch(cmd, argv+2, shm_basename, shm_initial_size);
}
