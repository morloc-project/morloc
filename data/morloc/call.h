#ifndef __MORLOC_CALL_H__
#define __MORLOC_CALL_H__

#include "memory.h"
#include <stdint.h>

typedef struct morloc_call_s {
    uint32_t midx;
    uint8_t** args;
    size_t nargs;
    int owns_args;
} morloc_call_t;

typedef struct client_list_s {
    int fd;
    struct client_list_s* next;
} client_list_t;

typedef struct language_daemon_s {
    char* socket_path;
    char* tmpdir;
    char* shm_basename;
    shm_t* shm;
    size_t shm_default_size;
    int server_fd;
    fd_set read_fds;
    client_list_t* client_fds;
} language_daemon_t;

// Nexus socket descriptor for a language pool
typedef struct morloc_socket_s {
    char* lang;
    char** syscmd;
    char* socket_filename;
    int pid;
} morloc_socket_t;

#endif // __MORLOC_CALL_H__
