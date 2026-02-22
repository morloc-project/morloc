#include "morloc.h"

void close_socket(int socket_id){
    if(socket_id >= 0) close(socket_id);
}

void close_daemon(language_daemon_t** daemon_ptr) {
    if (daemon_ptr && *daemon_ptr) {
        language_daemon_t* daemon = *daemon_ptr;

        close_socket(daemon->server_fd);

        // This list should always be empty if the run was successful
        client_list_t *current = daemon->client_fds;
        while (current) {
            client_list_t *next = current->next;
            close(current->fd);
            free(current);
            current = next;
        }

        unlink(daemon->socket_path);

        free(daemon->socket_path);
        free(daemon->tmpdir);
        free(daemon->shm_basename);

        free(daemon);
        *daemon_ptr = NULL;  // Safe nullification of caller's pointer
    }
}

static int new_socket(ERRMSG){
    INT_RETURN_SETUP

    int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    RAISE_IF(socket_fd == EXIT_FAIL, "Error creating socket")

    return socket_fd;
}

static struct sockaddr_un new_server_addr(const char* socket_path){
    // Set up the server address structure
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);
    return server_addr;
}

static int new_server(const char* socket_path, ERRMSG){
    INT_RETURN_SETUP

    int server_fd = TRY(new_socket);

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Remove any existing socket file
    unlink(socket_path);

    // Bind the socket to the address
    RAISE_IF_WITH(
        bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == EXIT_FAIL,
        close_socket(server_fd),
        "Error binding socket"
    )

    RAISE_IF_WITH(
        listen(server_fd, 16) == EXIT_FAIL,
        close_socket(server_fd),
        "Error listening on socket"
    )

    return server_fd;
}

language_daemon_t* start_daemon(
    const char* socket_path,
    const char* tmpdir,
    const char* shm_basename,
    size_t shm_default_size,
    ERRMSG
){
    PTR_RETURN_SETUP(language_daemon_t)

    language_daemon_t* daemon = (language_daemon_t*)calloc(1, sizeof(language_daemon_t));
    RAISE_IF(daemon == NULL, "Calloc for language_daemon_t failed")

    daemon->server_fd = -1; // prevent close_daemon from closing fd 0 on early error
    daemon->socket_path = strdup(socket_path);
    daemon->tmpdir = strdup(tmpdir);
    daemon->shm_basename = strdup(shm_basename);
    if (daemon->socket_path == NULL || daemon->tmpdir == NULL || daemon->shm_basename == NULL) {
        close_daemon(&daemon);
        RAISE("strdup failed in start_daemon")
    }
    daemon->shm_default_size = shm_default_size;

    // Initialize thread-shared resources
    daemon->client_fds = NULL;  // Explicit NULL initialization
    FD_ZERO(&daemon->read_fds); // Initialize descriptor set

    // Set fallback directory for file-backed shared memory when /dev/shm is too small
    shm_set_fallback_dir(tmpdir);

    // create the shared memory mappings
    daemon->shm = TRY_WITH(close_daemon(&daemon), shinit, shm_basename, 0, shm_default_size);

    // Setup a new daemon that uses a given path for the socket address
    daemon->server_fd = TRY_WITH(close_daemon(&daemon), new_server, socket_path);

    // Set the daemon socket to non-blocking mode (critical for pselect safety)
    int flags = fcntl(daemon->server_fd, F_GETFL);
    if (flags == -1 || fcntl(daemon->server_fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        close_daemon(&daemon);
        RAISE("Failed to set non-blocking mode: %s", strerror(errno));
    }

    return daemon;
}

uint8_t* stream_from_client_wait(int client_fd, int pselect_timeout_us, int recv_timeout_us, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    if (fcntl(client_fd, F_GETFD) == -1) {
        RAISE("Invalid file descriptor: %s", strerror(errno));
    }

    char* buffer = (char*)calloc(BUFFER_SIZE, sizeof(char));
    RAISE_IF(buffer == NULL, "calloc failed for buffer: %s", strerror(errno))

    fd_set read_fds;
    int max_fd = client_fd;

    // Timeout structure initialization
    struct timespec* timeout_loop_ptr = NULL;
    struct timespec ts_loop;
    if(pselect_timeout_us > 0) {
        ts_loop.tv_sec = pselect_timeout_us / 1000000;
        ts_loop.tv_nsec = (pselect_timeout_us % 1000000) * 1000;
        timeout_loop_ptr = &ts_loop;
    }

    // Signal masking setup
    sigset_t mask, origmask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT); // do we need to add more masks here?
    pthread_sigmask(SIG_SETMASK, &mask, &origmask);

    // Initial receive with timeout (retry on EINTR)
    int ready;
    do {
        FD_ZERO(&read_fds);
        FD_SET(client_fd, &read_fds);
        ready = pselect(max_fd + 1, &read_fds, NULL, NULL, timeout_loop_ptr, &origmask);
    } while (ready < 0 && errno == EINTR);
    pthread_sigmask(SIG_SETMASK, &origmask, NULL);

    RAISE_IF_WITH(ready == 0, free(buffer), "Timeout waiting for initial data");
    RAISE_IF_WITH(ready < 0, free(buffer), "pselect error: %s", strerror(errno));
    RAISE_IF_WITH(!FD_ISSET(client_fd, &read_fds), free(buffer), "Bad client file descriptor")
    ssize_t recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);

    RAISE_IF_WITH(recv_length == 0, free(buffer), "Connection closed by peer: %s", strerror(errno));
    RAISE_IF_WITH(recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN,
                  free(buffer), "Recv error: %s", strerror(errno));

    size_t packet_length = morloc_packet_size((uint8_t*)buffer, &CHILD_ERRMSG);
    if (CHILD_ERRMSG != NULL) {
        free(buffer);
        RAISE("Failed to get packet size: %s", CHILD_ERRMSG);
    }
    uint8_t* result = (uint8_t*)calloc(packet_length, sizeof(uint8_t));
    if (result == NULL) {
        free(buffer);
        RAISE("calloc failure: %s", strerror(errno));
    }

    uint8_t* data_ptr = result;
    memcpy(data_ptr, buffer, recv_length);
    data_ptr += recv_length;
    free(buffer);

    int attempts = 10;
    // Receive remaining data with per-operation timeout
    while ((size_t)(data_ptr - result) < packet_length) {
        bool packet_received = false;
        for(int packet_attempts = 0; packet_attempts < attempts; packet_attempts++) {
            FD_ZERO(&read_fds);
            FD_SET(client_fd, &read_fds);

            // Reset timeout for each iteration with linear backoff
            struct timespec ts_loop;
            if(recv_timeout_us > 0) {
                long total_us = (long)recv_timeout_us * (packet_attempts + 1);
                ts_loop.tv_sec = total_us / 1000000;
                ts_loop.tv_nsec = (total_us % 1000000) * 1000L;
                timeout_loop_ptr = &ts_loop;
            }

            pthread_sigmask(SIG_SETMASK, &mask, NULL);
            ready = pselect(max_fd + 1, &read_fds, NULL, NULL, timeout_loop_ptr, &origmask);
            pthread_sigmask(SIG_SETMASK, &origmask, NULL);

            RAISE_IF_WITH(ready == 0, free(result), "Timeout waiting for remaining data");
            RAISE_IF_WITH(ready < 0 && errno != EINTR, free(result), "pselect error: %s", strerror(errno));
            if (ready <= 0) continue;

            if (FD_ISSET(client_fd, &read_fds)) {
                recv_length = recv(client_fd, data_ptr, BUFFER_SIZE, 0);
                if (recv_length > 0) {
                    data_ptr += recv_length;
                    packet_received = true;
                    break;
                }
                RAISE_IF_WITH(recv_length == 0, free(result), "Connection closed early: %s", strerror(errno));
                RAISE_IF_WITH(recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN,
                              free(result), "Recv error: %s", strerror(errno));
            }
        }
        RAISE_IF_WITH(!packet_received, free(result), "Failed to retrieve packet")
    }

    return result;
}

uint8_t* stream_from_client(int client_fd, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(stream_from_client_wait, client_fd, 0, 0);
    return packet;
}

uint8_t* send_and_receive_over_socket_wait(const char* socket_path, const uint8_t* packet, int pselect_timeout_us, int recv_timeout_us, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    int client_fd = TRY(new_socket);

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Data packet to return
    uint8_t* result = NULL;

    // Connect to the server
    int retcode = -1;
    WAIT( { retcode = connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)); },
          retcode == 0,
          RAISE_WITH(close_socket(client_fd), "Failed to connect to pipe '%s', ran out of time", socket_path)
        )

    size_t packet_size = TRY(morloc_packet_size, packet);

    // Send data in a loop to handle partial sends for large packets
    size_t total_sent = 0;
    while (total_sent < packet_size) {
        ssize_t bytes_sent = send(client_fd, packet + total_sent, packet_size - total_sent, MSG_NOSIGNAL);
        if (bytes_sent <= 0) {
            close_socket(client_fd);
            RAISE("Failed to send data to '%s': %s", socket_path, strerror(errno));
        }
        total_sent += (size_t)bytes_sent;
    }

    result = stream_from_client_wait(client_fd, pselect_timeout_us, recv_timeout_us, &CHILD_ERRMSG);
    RAISE_IF_WITH(CHILD_ERRMSG != NULL, close_socket(client_fd), "Failed to read data returned from pipe '%s'\n%s", socket_path, CHILD_ERRMSG);

    close_socket(client_fd);

    return result;
}

uint8_t* send_and_receive_over_socket(const char* socket_path, const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* result = TRY(send_and_receive_over_socket_wait, socket_path, packet, 0, 0);
    return result;
}

size_t send_packet_to_foreign_server(int client_fd, uint8_t* packet, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0)

    size_t size = TRY(morloc_packet_size, packet);

    size_t total_sent = 0;
    while (total_sent < size) {
        ssize_t bytes_sent = send(client_fd, packet + total_sent, size - total_sent, MSG_NOSIGNAL);
        RAISE_IF(bytes_sent <= 0, "Failed to send over client %d: %s", client_fd, strerror(errno))
        total_sent += (size_t)bytes_sent;
    }

    return total_sent;
}

int wait_for_client_with_timeout(language_daemon_t* daemon, int timeout_us, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)

    // clear the list of file descriptors, these will be re-added below
    FD_ZERO(&daemon->read_fds);

    // add the server file descriptor to the fds_set
    FD_SET(daemon->server_fd, &daemon->read_fds);

    int max_fd = daemon->server_fd;

    client_list_t* client_fds;

    // loop through all pre-existing the client file descriptors stored in the daemon list
    for(client_fds = daemon->client_fds; client_fds != NULL; client_fds = client_fds->next){
        FD_SET(client_fds->fd, &daemon->read_fds);
        max_fd = max_fd > client_fds->fd ? max_fd : client_fds->fd;
    }

    // Modified WAIT block using pselect

    // Timeout structure initialization
    struct timespec* timeout_ptr = NULL;
    struct timespec ts_loop;
    if(timeout_us > 0) {
        ts_loop.tv_sec = timeout_us / 1000000;
        ts_loop.tv_nsec = (timeout_us % 1000000) * 1000;
        timeout_ptr = &ts_loop;
    }

    int ready;
    sigset_t emptymask;
    sigemptyset(&emptymask);
    // `pselect` will until timeout for something to crawl out of the pipe.
    // But if the pipe itself is missing or broken (for example, if the socket
    // file has not yet been written), then select dies immediately, for this
    // reason it is wrapped in WAIT to retry for a few minutes for giving up the
    // ghost for good.
    ready = pselect(max_fd + 1, &daemon->read_fds, NULL, NULL, timeout_ptr, &emptymask);
    if (ready < 0) {
        if (errno == EINTR) return 0;
        RAISE("pselect error: %s", strerror(errno));
    }

    // if pselect timed out, return 0
    if(ready == 0){
        return ready;
    }

    int selected_fd = -1;

    // get the new client that select found and add it to the end of the client list
    if (FD_ISSET(daemon->server_fd, &daemon->read_fds)) {
        selected_fd = accept(daemon->server_fd, NULL, NULL);
        if (selected_fd >= 0) {
            fcntl(selected_fd, F_SETFL, O_NONBLOCK);

            // Create the new client
            client_list_t* new_client = (client_list_t*)calloc(1, sizeof(client_list_t));
            new_client->fd = selected_fd;
            new_client->next = NULL;

            if(daemon->client_fds == NULL){
                daemon->client_fds = new_client;
            } else {
                client_list_t* last = daemon->client_fds;
                while (last->next) last = last->next;  // Find last node
                last->next = new_client;
            }
        } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
            RAISE("Error accepting client connection");
        }
    }

    if (daemon->client_fds == NULL) {
        return 0; // no client ready (spurious wakeup), caller should retry
    }

    client_fds = daemon->client_fds;
    int return_fd = client_fds->fd;
    daemon->client_fds = daemon->client_fds->next;
    free(client_fds);

    return return_fd;
}

int wait_for_client(language_daemon_t* daemon, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)
    int return_fd = TRY(wait_for_client_with_timeout, daemon, 0);
    return return_fd;
}
