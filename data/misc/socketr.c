#include <R.h>
#include <Rinternals.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/select.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <poll.h>


/* Fuck it, I can't get streaming to work properly here So I'll just make the      */
/* buffer big enough to hold anything This isn't as bad as it sounds, since really */
/* large objects are written to files.                                             */
#define BUFFER_SIZE 1048608

#define LOGFILE "log"

#define PACKET_TYPE_DATA  0x00
#define PACKET_TYPE_CALL  0x01
#define PACKET_TYPE_GET   0x02
#define PACKET_TYPE_PUT   0x03
#define PACKET_TYPE_PING  0x04

#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_MMAP  0x02 // the message is a memory mapped file

#define PACKET_FORMAT_JSON  0x00

#define PACKET_COMPRESSION_NONE  0x00 // uncompressed
#define PACKET_ENCRYPTION_NONE   0x00 // unencrypted

#define PACKET_STATUS_PASS  0x00
#define PACKET_STATUS_FAIL  0x01

struct Message {
  char* data;
  uint64_t length;
};

struct Header {
    uint32_t offset;
    uint64_t length;
};


int log_message(const char *message) {
    FILE *file = fopen(LOGFILE, "a");
    
    if (file == NULL) {
        printf("R: Error opening file %s\n", LOGFILE);
        return -1;
    }
    
    fprintf(file, "R socket: %s", message);
    
    fclose(file);
    return 0;
}

void socket_close(int socket_fd, const char *desc){
    char msg[256];
    sprintf(msg, "Closing %s socket number %d\n", desc, socket_fd);
    log_message(msg); 
    close(socket_fd);
}

// Create a Unix domain socket
SEXP R_new_socket() {
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (socket_fd == -1) {
        log_message("Error creating socket");
        INTEGER(result)[0] = -1;
    } else {
        INTEGER(result)[0] = socket_fd;
    }
    UNPROTECT(1);
    return result;
}

SEXP R_set_nonblocking(SEXP socket_fd) {
    int fd = asInteger(socket_fd);
    int flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    return R_NilValue;
}

SEXP R_new_server_addr(SEXP R_SOCKET_PATH) {
    const char* SOCKET_PATH = CHAR(asChar(R_SOCKET_PATH));
    SEXP result = PROTECT(allocVector(RAWSXP, sizeof(struct sockaddr_un)));
    struct sockaddr_un* server_addr = (struct sockaddr_un*)RAW(result);
    memset(server_addr, 0, sizeof(struct sockaddr_un));
    server_addr->sun_family = AF_UNIX;
    strncpy(server_addr->sun_path, SOCKET_PATH, sizeof(server_addr->sun_path) - 1);
    UNPROTECT(1);
    return result;
}

SEXP R_new_server(SEXP R_SOCKET_PATH) {
    const char* SOCKET_PATH = CHAR(asChar(R_SOCKET_PATH));
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    int server_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    int flags = fcntl(server_fd, F_GETFL, 0);
    fcntl(server_fd, F_SETFL, flags | O_NONBLOCK);

    if (server_fd == -1) {
        log_message("Error creating socket");
        INTEGER(result)[0] = -1;
        UNPROTECT(1);
        return result;
    }
    
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, SOCKET_PATH, sizeof(server_addr.sun_path) - 1);
    
    unlink(SOCKET_PATH);
    
    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        log_message("Error binding socket");
        socket_close(server_fd, "failing server");
        INTEGER(result)[0] = -1;
        UNPROTECT(1);
        return result;
    }
    
    if (listen(server_fd, 1) == -1) {
        log_message("Error listening on socket");
        socket_close(server_fd, "failing server");
        INTEGER(result)[0] = -1;
        UNPROTECT(1);
        return result;
    }
    
    INTEGER(result)[0] = server_fd;
    UNPROTECT(1);
    return result;
}

SEXP R_accept_client(SEXP R_server_fd, SEXP R_timeout) {
    int server_fd = INTEGER(R_server_fd)[0];
    double timeout = REAL(R_timeout)[0];
    
    fd_set readfds;
    struct timeval tv;
    
    FD_ZERO(&readfds);
    FD_SET(server_fd, &readfds);
    
    // convert a decimal representation of time in seconds to seconds + microseconds
    tv.tv_sec = (int)timeout;
    tv.tv_usec = (int)((timeout - (int)timeout) * 1e6);
    
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    
    int ready = select(server_fd + 1, &readfds, NULL, NULL, &tv);
    
    if (ready == -1) {
        // Error occurred
        INTEGER(result)[0] = -1;
    } else if (ready == 0) {
        // Timeout occurred
        INTEGER(result)[0] = -2;
    } else {
        // A client is ready to be accepted
        INTEGER(result)[0] = accept(server_fd, NULL, NULL);
    }
    
    UNPROTECT(1);
    return result;
}

uint64_t read_uint64(const char* bytes, size_t offset) {
    uint64_t x = 0;
    for (size_t i = 0; i < 8; i++) {
        uint64_t multiplier = 1ULL << (8 * (7 - i));
        x += (uint64_t)(unsigned char)bytes[i + offset] * multiplier;
    }
    return x;
}

uint32_t read_uint32(const char* bytes, size_t offset) {
    uint32_t x = 0;
    for (size_t i = 0; i < 4; i++) {
        uint32_t multiplier = 1U << (8 * (3 - i));
        x += (uint32_t)(unsigned char)bytes[i + offset] * multiplier;
    }
    return x;
}

struct Header read_header(const char* msg) {
    struct Header header;

    header.offset = read_uint32(msg, 20);
    header.length = read_uint64(msg, 24);

    if (!((unsigned char)msg[0] == 0x6d && 
          (unsigned char)msg[1] == 0xf8 && 
          (unsigned char)msg[2] == 0x07 && 
          (unsigned char)msg[3] == 0x07)) {
        error("Bad magic in R socket.h. Expected: 6d f8 07 07, Got: %02x %02x %02x %02x. Calculated length: %llu, offset: %u",
          (unsigned char)msg[0], (unsigned char)msg[1], 
          (unsigned char)msg[2], (unsigned char)msg[3],
          (unsigned long long)header.length, header.offset
        );
    }

    return header;
}

struct Message stream_recv(int client_fd) {
    char logmsg[256];
    struct Message result;
    result.length = 0;
    result.data = NULL;

    char* buffer = (char*)malloc(BUFFER_SIZE * sizeof(char));
    if (!buffer) {
        error("Failed to allocate memory for buffer");
    }

    struct pollfd pfd;
    pfd.fd = client_fd;
    pfd.events = POLLIN;

    sprintf(logmsg, "Polling %d for first packet\n", client_fd);
    log_message(logmsg); 

    // Receive the first part of the response, this will include the header
    ssize_t recv_length = 0;
    while (recv_length == 0) {
        int poll_result = poll(&pfd, 1, -1);
        if (poll_result < 0) {
            free(buffer);
            error("Poll failed");
        }

        recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);
        if (recv_length < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                recv_length = 0;  // Try again
            } else {
                free(buffer);
                error("Failed to receive data from socket");
            }
        }
    }

    // Parse the header, from this we learn the expected size of the packet
    struct Header header = read_header(buffer);

    result.length = 32 + header.offset + header.length;

    if(result.length > BUFFER_SIZE){
        free(buffer);
        error("Data exceeds buffer size and streaming is not supported for R");
    }

    // Allocate enough memory to store the entire packet
    result.data = (char*)malloc(result.length * sizeof(char));
    if (!result.data) {
        free(buffer);
        error("Failed to allocate memory for result data");
    }

    // Copy the already received data
    memcpy(result.data, buffer, recv_length);

    return result;
}




SEXP get(int client_fd) {
    char logmsg[256];
    struct Message received_data;

    sprintf(logmsg, "Entering get with client %d\n", client_fd);
    log_message(logmsg); 

    received_data = stream_recv(client_fd);

    if (received_data.data == NULL || received_data.length <= 0) {
        error("Failed to receive data or received empty data");
    }

    sprintf(logmsg, "Retrieved %ld bytes. First few bytes: %02x %02x %02x %02x\n", 
            received_data.length, 
            received_data.data[0], received_data.data[1], 
            received_data.data[2], received_data.data[3]);
    log_message(logmsg);

    SEXP data = PROTECT(allocVector(RAWSXP, received_data.length));
    if (data == R_NilValue) {
        free(received_data.data);
        error("Failed to allocate memory for R vector");
    }

    memcpy(RAW(data), received_data.data, received_data.length);

    SEXP message = PROTECT(allocVector(VECSXP, 2));
    if (message == R_NilValue) {
        UNPROTECT(2);
        free(received_data.data);
        error("Failed to allocate memory for R list");
    }

    SEXP length = PROTECT(allocVector(INTSXP, 1));
    if (length == R_NilValue) {
        UNPROTECT(3);
        free(received_data.data);
        error("Failed to allocate memory for R integer");
    }

    INTEGER(length)[0] = received_data.length;
    
    SET_VECTOR_ELT(message, 0, data);
    SET_VECTOR_ELT(message, 1, length);

    free(received_data.data);
    
    UNPROTECT(3);
    return message;
}


SEXP R_get(SEXP r_client_fd) {
    return get(INTEGER(r_client_fd)[0]);
}


ssize_t send_all_data(int client_fd, const char* data, size_t length) {
    ssize_t total_sent = 0;
    const char* current_position = data;
    
    while (total_sent < length) {
        size_t remaining = length - total_sent;
        size_t to_send = (remaining < BUFFER_SIZE) ? remaining : BUFFER_SIZE;
        
        ssize_t sent = send(client_fd, current_position, to_send, 0);
        
        if (sent == -1) {
            if (errno == EINTR) {
                // Interrupted by signal, retry
                continue;
            }
            // Handle other errors
            return -1;
        } else if (sent == 0) {
            // Connection closed
            return total_sent;
        }
        
        total_sent += sent;
        current_position += sent;
    }
    
    return total_sent;
}

void send_all_data_or_die(int client_fd, const char* data, size_t length) {
    ssize_t nbytes_sent = send_all_data(client_fd, data, length);

    if (nbytes_sent < 0){
        error("Failed to start sending data");
    }

    if (nbytes_sent < length){
        error("Failed to send full message");
    }
}


SEXP R_send_data(SEXP R_client_fd, SEXP R_msg) {
    int client_fd = INTEGER(R_client_fd)[0];
    SEXP data = VECTOR_ELT(R_msg, 0);
    SEXP length = VECTOR_ELT(R_msg, 1);

    send_all_data_or_die(client_fd, RAW(data), INTEGER(length)[0]);

    return R_NilValue;
}

SEXP R_close_socket(SEXP R_socket_fd) {
    int socket_fd = INTEGER(R_socket_fd)[0];
    socket_close(socket_fd, "unknown (from R command)");
    return R_NilValue;
}



SEXP R_ask(SEXP R_socket_path, SEXP R_message) {
    const char* socket_path = CHAR(asChar(R_socket_path));
    char* data = RAW(VECTOR_ELT(R_message, 0));
    size_t length = INTEGER(VECTOR_ELT(R_message, 1))[0];
    
    int client_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    if (client_fd == -1) {
        Rf_error("Error creating socket\n");
        return R_NilValue; // never reached
    }

    int flags = fcntl(client_fd, F_GETFL, 0);
    fcntl(client_fd, F_SETFL, flags | O_NONBLOCK);
    
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);

    if (connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        socket_close(client_fd, "failing ask client");
        Rf_error("Empty message");
        return R_NilValue; // never reached
    }
    
    send_all_data_or_die(client_fd, data, length);

    SEXP result = get(client_fd);
    
    socket_close(client_fd, "ask client");
    
    return result;
}

static const R_CallMethodDef CallEntries[] = {
    {"R_new_socket", (DL_FUNC) &R_new_socket, 0},
    {"R_new_server_addr", (DL_FUNC) &R_new_server_addr, 1},
    {"R_new_server", (DL_FUNC) &R_new_server, 1},
    {"R_accept_client", (DL_FUNC) &R_accept_client, 1},
    {"R_get", (DL_FUNC) &R_get, 1},
    {"R_send_data", (DL_FUNC) &R_send_data, 2},
    {"R_close_socket", (DL_FUNC) &R_close_socket, 1},
    {"R_ask", (DL_FUNC) &R_ask, 2},
    {NULL, NULL, 0}
};

void R_init_mysocketlib(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
