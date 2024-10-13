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


#define BUFFER_SIZE 4096

#define PACKET_TYPE_DATA  0x00
#define PACKET_TYPE_CALL  0x01
#define PACKET_TYPE_GET   0x02
#define PACKET_TYPE_PUT   0x03
#define PACKET_TYPE_PING  0x04

#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_NXDB  0x02 // the message is a key to the nexus uses to access the data

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

// Create a Unix domain socket
SEXP R_new_socket() {
    SEXP result = PROTECT(allocVector(INTSXP, 1));
    int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (socket_fd == -1) {
        REprintf("Error creating socket\n");
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
    if (server_fd == -1) {
        REprintf("Error creating socket\n");
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
        REprintf("Error binding socket\n");
        close(server_fd);
        INTEGER(result)[0] = -1;
        UNPROTECT(1);
        return result;
    }
    
    if (listen(server_fd, 1) == -1) {
        REprintf("Error listening on socket\n");
        close(server_fd);
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
    struct Message result;
    result.length = 0;
    result.data = NULL;

    char* buffer = (char*)malloc(BUFFER_SIZE * sizeof(char));
    if (!buffer) {
        error("Failed to allocate memory for buffer");
    }

    // Receive the first part of the response, this will include the header
    ssize_t recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);
    if (recv_length <= 0) {
        free(buffer);
        error("Failed to receive data from socket");
    }

    // Parse the header, from this we learn the expected size of the packet
    struct Header header = read_header(buffer);
    
    result.length = 32 + header.offset + header.length;

    // Allocate enough memory to store the entire packet
    result.data = (char*)malloc(result.length * sizeof(char));
    if (!result.data) {
        free(buffer);
        error("Failed to allocate memory for result data");
    }

    char* data_ptr = result.data;
    memcpy(data_ptr, buffer, recv_length);
    data_ptr += recv_length;

    free(buffer);

    // read in buffers of data until all data is received
    while ((size_t)(data_ptr - result.data) < result.length) {
        recv_length = recv(client_fd, data_ptr, BUFFER_SIZE, 0);
        if (recv_length <= 0) {
            free(result.data);
            error("Failed to receive complete data from socket");
        }
        data_ptr += recv_length;
    }

    return result;
}


SEXP get(int client_fd) {
    struct Message received_data = stream_recv(client_fd);

    SEXP data = PROTECT(allocVector(RAWSXP, received_data.length));
    if (data == R_NilValue) {
        free(received_data.data);
        error("Failed to allocate memory for R vector");
    }

    memcpy(RAW(data), received_data.data, received_data.length);

    SEXP message = PROTECT(allocVector(VECSXP, 2));
    if (message == R_NilValue) {
        UNPROTECT(1);
        free(received_data.data);
        error("Failed to allocate memory for R list");
    }

    SEXP length = PROTECT(allocVector(INTSXP, 1));
    if (length == R_NilValue) {
        UNPROTECT(2);
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

SEXP R_send_data(SEXP R_client_fd, SEXP R_msg) {
    int client_fd = INTEGER(R_client_fd)[0];
    SEXP data = VECTOR_ELT(R_msg, 0);
    SEXP length = VECTOR_ELT(R_msg, 1);
    
    send(client_fd, RAW(data), INTEGER(length)[0], 0);
    
    return R_NilValue;
}

SEXP R_close_socket(SEXP R_socket_fd) {
    int socket_fd = INTEGER(R_socket_fd)[0];
    close(socket_fd);
    return R_NilValue;
}

SEXP R_ask(SEXP R_socket_path, SEXP R_message) {
    const char* socket_path = CHAR(asChar(R_socket_path));
    SEXP data = VECTOR_ELT(R_message, 0);
    SEXP length = VECTOR_ELT(R_message, 1);
    
    int client_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (client_fd == -1) {
        Rf_error("Error creating socket\n");
        return R_NilValue; // never reached
    }
    
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);
    
    if (connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        close(client_fd);
        Rf_error("Empty message");
        return R_NilValue; // never reached
    }
    
    send(client_fd, RAW(data), INTEGER(length)[0], 0);

    SEXP result = get(client_fd);
    
    close(client_fd);
    
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
