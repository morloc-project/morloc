#include <R.h>
#include <Rinternals.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/select.h>
#include <errno.h>


#define BUFFER_SIZE 1024

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


SEXP R_get(SEXP R_client_fd) {
    int client_fd = INTEGER(R_client_fd)[0];
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SEXP data = PROTECT(allocVector(RAWSXP, BUFFER_SIZE));
    SEXP length = PROTECT(allocVector(INTSXP, 1));
    
    INTEGER(length)[0] = recv(client_fd, RAW(data), BUFFER_SIZE, 0);
    
    SET_VECTOR_ELT(result, 0, data);
    SET_VECTOR_ELT(result, 1, length);
    
    UNPROTECT(3);
    return result;
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
    
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SEXP result_data = PROTECT(allocVector(RAWSXP, BUFFER_SIZE));
    SEXP result_length = PROTECT(allocVector(INTSXP, 1));
    
    int client_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (client_fd == -1) {
        REprintf("Error creating socket\n");
        INTEGER(result_length)[0] = 0;
        SET_VECTOR_ELT(result, 0, result_data);
        SET_VECTOR_ELT(result, 1, result_length);
        UNPROTECT(3);
        return result;
    }
    
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);
    
    if (connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        REprintf("Error connecting to server\n");
        close(client_fd);
        INTEGER(result_length)[0] = 0;
        SET_VECTOR_ELT(result, 0, result_data);
        SET_VECTOR_ELT(result, 1, result_length);
        UNPROTECT(3);
        return result;
    }
    
    send(client_fd, RAW(data), INTEGER(length)[0], 0);
    
    INTEGER(result_length)[0] = recv(client_fd, RAW(result_data), BUFFER_SIZE, 0);
    
    close(client_fd);
    
    SET_VECTOR_ELT(result, 0, result_data);
    SET_VECTOR_ELT(result, 1, result_length);
    
    UNPROTECT(3);
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
