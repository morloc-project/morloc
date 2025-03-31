#include "morloc.h"

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>


// The maximum allowed number of language servers
//
// Currently set to the number of supported languages. This will of course need
// to be updated as more languages are added.
#define MAX_DAEMONS 3

int daemon_pids[MAX_DAEMONS] = { 0 }; 
char tmpdir[MAX_FILENAME_SIZE];

void clean_exit(int exit_code){
    // TODO: kill children
    // TODO: delete tmp files
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
        RAISE("execvp failed") // Only reached if exec fails
    } else if (pid > 0) { // Parent process
        return pid;
    } else {
        RAISE("fork failed");
    }
}

uint8_t* prepare_call_packet(uint32_t mid, const char** args, const Schema** arg_schemas){
    // TODO: complete
    return NULL;
}

uint8_t* client(morloc_socket_t root_socket, uint8_t* message){
    // TODO complete
    return NULL;
}

void print_return(uint8_t* packet, Schema* schema){
    char* child_errmsg = NULL;

    bool readable = get_morloc_data_packet_error_message(packet, &child_errmsg);
    // print error due to malformed packet
    if(!readable){
        if(child_errmsg == NULL){
            fprintf(stderr, "Failed to read returned data packet\n");
        } else {
            fprintf(stderr, "Failed to read returned data packet:\n%s\n", child_errmsg);
        }
        clean_exit(1);
    }

    // print error raised from inside morloc
    if(readable && child_errmsg != NULL){
        fprintf(stderr, child_errmsg);
        clean_exit(1);
    }

    // print result
    print_voidstar(packet, schema, &child_errmsg);
    if(child_errmsg == NULL){
        clean_exit(0);
    } else {
        fprintf(stderr, "Failed to print return packet:\n%s\n", child_errmsg);
        clean_exit(1);
    }
}

void run_command(
    uint32_t mid,
    const char** args,
    const char** arg_schema_strs,
    const char* return_schema_str,
    morloc_socket_t root_socket,
    morloc_socket_t** all_sockets, // not const, these are modified to add pid
    const char* tmpdir
){
    int pid = 0;
    char* errmsg = NULL;

    Schema* return_schema = parse_schema(&return_schema_str, &errmsg);
    if(errmsg != NULL){
        fprintf(stderr, "Failed to parse return schema");
    }

    for(size_t i = 0; all_sockets[i] != NULL; i++){
        all_sockets[i]->pid = start_language_server(all_sockets[i], &errmsg);
        if(errmsg != NULL){
            fprintf(stderr, errmsg);
            clean_exit(1);
        }
    }

    size_t nschemas = 0;
    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        nschemas++;
    }

    const Schema** arg_schemas = (const Schema**)calloc(nschemas + 1, sizeof(Schema*));
    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        arg_schemas[i] = parse_schema(&arg_schema_strs[i], &errmsg);
        if(errmsg != NULL){
            fprintf(stderr, errmsg);
            clean_exit(1);
        }
    }
    arg_schemas[nschemas] = NULL;
    

    uint8_t* call_packet = prepare_call_packet(mid, args, arg_schemas);

    uint8_t* result_packet = client(root_socket, call_packet);

    print_return(result_packet, return_schema);
}


void usage(){
    // <<<BREAK>>>
    clean_exit(0);
}

int dispatch(
    const char* cmd,
    const char** args,
    const char* tmpdir,
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

    if(argc == 1 || is_help(argv[1]) ){
        usage();
        return 1;
    }

    uint64_t job_hash = make_job_hash(42);

    size_t shm_initial_size = 0xffff;
    char shm_basename[MAX_FILENAME_SIZE];
    snprintf(shm_basename, sizeof(shm_basename), "morloc-%llu", job_hash);

    shm_t* shm = shinit(shm_basename, 0, shm_initial_size, &errmsg);
    if(errmsg != NULL){
        fprintf(stderr, errmsg);
        return 1;
    }

    char* cmd = argv[1];

    char* tmpdir = make_tmpdir(&errmsg);
    if(errmsg != NULL){
        fprintf(stderr, errmsg);
        return 1;
    }

    size_t nargs = argc - 2;
    const char* args[nargs];
    for (size_t i = 0; i < nargs; i++) {
        args[i] = argv[i + 2];  // Copy pointers as const
    }

    return dispatch(cmd, args, tmpdir, shm_basename, shm_initial_size);
}
