#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <algorithm> // for std::transform
#include <stdexcept>
#include <fstream>
#include <system_error>
#include <sys/stat.h>
#include <sys/mman.h>

// needed for foreign interface
#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <unistd.h>

// needed for interop
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/select.h>
#include <cmath>
#include <iomanip>
#include <poll.h>
#include <errno.h>
#include <fcntl.h>

#include <limits>
#include <tuple>
#include <utility>

using namespace std;

// global tmpdir value that will be set in main
std::string g_tmpdir;

// AUTO include statements start
// <<<BREAK>>>
// AUTO include statements end

// Proper linking of cppmorloc requires it be included AFTER the custom modules
#include "cppmorloc.hpp"

#define PROPAGATE_ERROR(errmsg) \
    if(errmsg != NULL) { \
      char errmsg_buffer[MAX_ERRMSG_SIZE] = { 0 }; \
      snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error C++ pool (%s:%d in %s):\n%s" , __FILE__, __LINE__, __func__, errmsg); \
      free(errmsg); \
      throw std::runtime_error(errmsg_buffer); \
    }

// AUTO serialization statements start
// <<<BREAK>>>
// AUTO serialization statements end



#define BUFFER_SIZE 4096


struct Message {
  char* data;
  uint64_t length;
};

// Function to log messages
template <class M>
void log_message(M message) {
    std::ofstream log_file("log", std::ios_base::app);
    log_file << "C: " << message << std::endl;
}

void socket_close(int socket_id, const std::string& desc){
    log_message("Closing " + desc + " socket fd for " + std::to_string(socket_id));
    close(socket_id);
}

// Interop /////////////

Message fail_packet(const char* errmsg){
    Message message;

    uint8_t* packet = make_fail_packet("Failed to send data");
    message.data = (char*)packet;

    char** child_errmsg = NULL;
    message.length = morloc_packet_size(packet, child_errmsg);
    PROPAGATE_ERROR(child_errmsg)

    return message;
}



// Functions used in making foreign calls

std::string generateTempFilename() {
    // Combine tmpdir with a template name
    std::string template_str = g_tmpdir + "/cpp_pool_XXXXXX";

    // Convert to a non-const char array (required by mkstemp)
    std::vector<char> template_char(template_str.begin(), template_str.end());
    template_char.push_back('\0'); // Ensure null-termination

    // Call mkstemp
    int fd = mkstemp(template_char.data());
    if (fd == -1) {
        throw std::runtime_error("Error generating temporary filename");
    }

    // Close the file descriptor
    close(fd);

    // Convert back to string and return
    return std::string(template_char.data());
}


// Transforms a serialized value into a message ready for the socket
template <typename T>
Message _put_value(const T& value, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    // toAnything writes to the shared memory volume
    void* voidstar = toAnything(schema, value);

    // convert to a relative pointer conserved between language servers
    relptr_t relptr = abs2rel_cpp(voidstar);

    uint8_t* packet = make_relptr_data_packet(relptr, schema);

    Message message;
    message.data = (char*)packet;

    char** errmsg = NULL;
    message.length = morloc_packet_size(packet, errmsg);
    PROPAGATE_ERROR(errmsg)

    return message;
}


// Use a key to retrieve a value
template <typename T>
T _get_value(const Message& packet, const std::string& schema_str){

    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    char* errmsg = nullptr;
    char* child_errmsg = nullptr;
    uint8_t* voidstar = (uint8_t*)get_morloc_data_packet_value((uint8_t*)packet.data, schema, &child_errmsg);
    if(voidstar == nullptr){
        std::string errmsg = strdup(child_errmsg);
        FREE(child_errmsg);
        throw std::runtime_error("Error in _get_value:\n" + errmsg);
    }

    T* dumby = nullptr;
    return fromAnything(schema, (void*)voidstar, dumby);
}



// Create a Unix domain socket
int new_socket(){
    int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    // AF_UNIX: Unix domain socket - other possibilities include:
    //  * AF_INET: For IPv4 Internet protocols - with SOCK_STREAM for TCP or
    //             with SOCK_DGRAM for UDP
    //  * AF_INET6: For IPv6 Internet protocols
    //  * AF_NETLINK: For kernel user interface device
    //  * AF_PACKET: For low-level packet interface

    // SOCK_STREAM - a stream socket that provides two-way, connection-based communication
    //  Alternatives include:
    //  * SOCK_DGRAM: For datagram (connectionless) sockets
    //  * SOCK_RAW: For raw network protocol access
    //  * SOCK_SEQPACKET: For sequential, reliable, connection-based packet streams

    // The 3rd argument, 0, is the protocol. For domain sockets there is only
    // one protocol, so this is always 0.

    if (socket_fd == -1) {
        std::cerr << "Error creating socket\n";
        return 1;
    }

    return socket_fd;
}

struct sockaddr_un new_server_addr(const char* socket_path){
    // Set up the server address structure
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);
    return server_addr;
}

int new_server(const char* socket_path){

    int server_fd = new_socket();

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Remove any existing socket file
    unlink(socket_path);

    // Bind the socket to the address
    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        log_message("Error binding socket");
        socket_close(server_fd, "failed server");
        return 1;
    }

    // Listen for connections
    if (listen(server_fd, 1) == -1) {
        log_message("Error listening on socket");
        socket_close(server_fd, "failed server");
        return 1;
    }

    return server_fd;
}

Message stream_recv(int client_fd) {
    struct Message result;
    result.length = 0;

    char* buffer = (char*)malloc(BUFFER_SIZE * sizeof(char));
    ssize_t recv_length;

    struct pollfd pfd;
    pfd.fd = client_fd;
    pfd.events = POLLIN;

    // Receive the first part of the response, this will include the header
    while (1) {
        int poll_result = poll(&pfd, 1, -1); // Wait indefinitely

        if (poll_result < 0) {
            if (errno == EINTR) continue; // Interrupted system call, retry
            log_message("Poll error: " + std::string(strerror(errno)));
            free(buffer);
            return result; // Return empty result on error
        }

        if (pfd.revents & POLLIN) {
            recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);
            if (recv_length > 0) break;
            if (recv_length == 0) {
                log_message("Connection closed by peer");
                free(buffer);
                return result; // Return empty result if connection closed
            }
            if (recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN) {
                log_message("Recv error: " + std::string(strerror(errno)));
                free(buffer);
                return result; // Return empty result on error
            }
        }
    }

    log_message("recv_length " + std::to_string(recv_length) + " from client_fd " + std::to_string(client_fd));

    // Parse the header, from this we learn the expected size of the packet
    char** errmsg = NULL;
    result.length = morloc_packet_size((uint8_t*)buffer, errmsg);
    PROPAGATE_ERROR(errmsg)

    // Allocate enough memory to store the entire packet
    result.data = (char*)malloc(result.length * sizeof(char));

    log_message("result.length " + std::to_string(result.length));

    // Create a pointer to the current writing index
    char* data_ptr = result.data;

    // copy data from the buffer to the message
    memcpy(data_ptr, buffer, recv_length);
    data_ptr += recv_length;

    // we don't need the buffer anymore, we'll write directly into the msg
    free(buffer);

    // read in buffers of data until all data is received
    log_message("Remaining data = " + std::to_string(result.length - recv_length));
    while (data_ptr - result.data < result.length) {
        while (1) {
            int poll_result = poll(&pfd, 1, -1); // Wait indefinitely

            if (poll_result < 0) {
                if (errno == EINTR) continue; // Interrupted system call, retry
                log_message("Poll error: " + std::string(strerror(errno)));
                free(result.data);
                result.length = 0;
                return result; // Return empty result on error
            }

            if (pfd.revents & POLLIN) {
                recv_length = recv(client_fd, data_ptr, BUFFER_SIZE, 0);
                log_message("Read " + std::to_string(recv_length) + " bytes");
                if (recv_length > 0) {
                    data_ptr += recv_length;
                    break;
                }
                if (recv_length == 0) {
                    log_message("Connection closed by peer");
                    free(result.data);
                    result.length = 0;
                    return result; // Return partial result if connection closed
                }
                if (recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN) {
                    log_message("Recv error: " + std::string(strerror(errno)));
                    free(result.data);
                    result.length = 0;
                    return result; // Return empty result on error
                }
            }
        }
    }

    log_message("Done: read " + std::to_string(data_ptr - result.data) + " bytes");

    return result;
}

Message ask(const char* socket_path, const Message& message){
    int client_fd = new_socket();

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    Message result;

    // Connect to the server
    int retcode = connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr));

    // If the connection succeeded, request data
    if (retcode != -1) {
        // Send a message and wait for a reply from the nexus
        ssize_t bytes_sent = send(client_fd, message.data, message.length, 0);

        if (bytes_sent != message.length) {
            result = fail_packet("Failed to send data");
        } else {
            result = stream_recv(client_fd);
        }
    }
    // otherwise, create a DATA FAIL packet on failure
    else {
        result = fail_packet("Failed to access client");
    }

    socket_close(client_fd, "finished ask client");

    return result;
}

Message foreign_call(
    const std::string& socket_path,
    size_t mid,
    const std::vector<Message>& arg_keys
) {
    Message message;

    const uint8_t** args = (const uint8_t**)calloc(arg_keys.size(), sizeof(uint8_t*));
    for(size_t i = 0; i < arg_keys.size(); i++){
        args[i] = (uint8_t*)arg_keys[i].data;
    }

    char** errmsg = NULL;
    uint8_t* packet = make_morloc_call_packet(mid, args, arg_keys.size(), errmsg);
    PROPAGATE_ERROR(errmsg)

    message.data = (char*)packet;
    message.length = morloc_packet_size(packet, errmsg);
    PROPAGATE_ERROR(errmsg)

    Message result_msg = ask(socket_path.c_str(), message);

    return result_msg;
}



// AUTO signatures statements start
// <<<BREAK>>>
// AUTO signatures statements end



// AUTO manifolds statements start
// <<<BREAK>>>
// AUTO manifolds statements end



Message dispatch(const Message& msg){
    char** errmsg = NULL;
    morloc_packet_header_t* header = read_morloc_packet_header((uint8_t*)msg.data, errmsg);
    PROPAGATE_ERROR(errmsg)

    if (header->command.cmd_type.type == PACKET_TYPE_PING){
        return msg;
    } else if (header->command.cmd_type.type == PACKET_TYPE_CALL) {

        morloc_call_t* call_packet = read_morloc_call_packet((uint8_t*)msg.data, errmsg);

        uint32_t mid = call_packet->midx;

        std::vector<Message> args(n);
        for(size_t i = 0; i < call_packet->nargs; i++){
            Message arg_msg;
            arg_msg.length = morloc_packet_size(call_packet->args[i], errmsg);
            PROPAGATE_ERROR(errmsg)
            arg_msg.data = call_packet.args[i];
            args.push_back(arg_msg);
        }

        free(call_packet);

        try {

// AUTO dispatch statements start
// <<<BREAK>>>
// AUTO dispatch statements end


        } catch (const std::exception& e) {
            // Catch any exception derived from std::exception
            return fail_packet(e.what());
        } catch (...) {
            // Catch any other type of exception
            return fail_packet("An unknown error occurred");
        }

    }

    return fail_packet("In C++ pool, call failed");
}




// MAIN

struct Job {
    int client_fd;
    Message message;
    pid_t pid;

    Job(int fd, const Message& msg) : client_fd(fd), message(msg), pid(-1) { }
};

std::vector<Job> children;


void run_job(int client_fd) {
    log_message("Starting new job for client_fd: " + std::to_string(client_fd));

    // Pull data from the client
    struct Message data = stream_recv(client_fd);

    // Store the job info
    Job new_job(client_fd, data);

    // If any data was pulled, operate on it
    if (data.length > 0) {
        pid_t pid = fork();
        if (pid == 0) {
            // Entered by the child process
            log_message("Child process started for job");

            // Run the job
            Message result = dispatch(new_job.message);

            log_message("Job dispatched, result length:" + std::to_string(result.length));

            // return the result to the client
            size_t bytes_sent = send(new_job.client_fd, result.data, result.length, 0);

            if (bytes_sent != result.length) {
                log_message("Failed to send return result data");
            } else {
                log_message("Successfully sent " + std::to_string(bytes_sent) + " bytes to " + std::to_string(new_job.client_fd));
            }

            // close the current client
            socket_close(new_job.client_fd, "finished job client");

            // And exit the child
            exit(0);
        } else if (pid > 0) {
            // Entered by the parent process
            new_job.pid = pid;
            children.push_back(new_job);
            log_message("Parent process: added child with PID " + std::to_string(pid));
        } else {
            log_message("Fork failed");
            socket_close(client_fd, "failed job client");
        }
    } else {
        // Close the connection if no data was received
        log_message("No data received, closing connection");
        socket_close(client_fd, "failed job client");
    }
}


int main(int argc, char* argv[]) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " <socket_path>" << " <tmpdir>" << "<shm_basename>\n";
        return 1;
    }

    // path to the socket file
    const char* socket_path = argv[1];

    // global variable storing the main temporary directory
    g_tmpdir = std::string(argv[2]);

    // the basename for the shared memory files (i.e., in /dev/shm)
    const char* shm_basename = argv[3];

    // create the shared memory mappings
    shm_t* shm = shinit_cpp(shm_basename, 0, 0xffff);

    log_message("Server starting with socket path: " + std::string(socket_path));

    // Setup a new server that uses a given path for the socket address
    int server_fd = new_server(socket_path);

    // Set the server socket to non-blocking mode
    fcntl(server_fd, F_SETFL, O_NONBLOCK);

    log_message("Server socket created and set to non-blocking mode");

    fd_set read_fds;
    struct timeval tv;
    std::vector<int> client_fds;

    while (true) {
        FD_ZERO(&read_fds);
        FD_SET(server_fd, &read_fds);
        int max_fd = server_fd;

        for (int fd : client_fds) {
            FD_SET(fd, &read_fds);
            max_fd = std::max(max_fd, fd);
        }

        tv.tv_sec = 0;
        tv.tv_usec = 100; // 100 microseconds timeout

        int ready = select(max_fd + 1, &read_fds, NULL, NULL, &tv);

        if (ready == -1) {
            log_message("Error in select: " + std::string(strerror(errno)));
            continue;
        }

        if (FD_ISSET(server_fd, &read_fds)) {
            int client_fd = accept(server_fd, NULL, NULL);
            if (client_fd > 0) {
                log_message("Accepted new client connection: " + std::to_string(client_fd));
                fcntl(client_fd, F_SETFL, O_NONBLOCK);
                client_fds.push_back(client_fd);
            } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
                log_message("Error accepting client connection: " + std::string(strerror(errno)));
            }
        }

        for (auto it = client_fds.begin(); it != client_fds.end();) {
            int fd = *it;
            if (FD_ISSET(fd, &read_fds)) {
                run_job(fd);
                it = client_fds.erase(it);
            } else {
                ++it;
            }
        }

        // TODO: reap zombies
    }

    // Close the server
    socket_close(server_fd, "server");

    // Remove the socket file
    unlink(socket_path);
    log_message("Socket file removed");

    return 0;
}
