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

// Proper linking of cppmpack requires it be included AFTER the custom modules 
#include "cppmpack.hpp"


// AUTO serialization statements start
// <<<BREAK>>>
// AUTO serialization statements end



#define BUFFER_SIZE 4096

#define PACKET_TYPE_DATA  0x00
#define PACKET_TYPE_CALL  0x01
#define PACKET_TYPE_PING  0x02
#define PACKET_TYPE_GET   0x03
#define PACKET_TYPE_POST  0x04
#define PACKET_TYPE_PUT   0x05
#define PACKET_TYPE_DEL   0x06

#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_RPTR  0x02 // the message is a relative pointer to shared memory

#define PACKET_FORMAT_JSON     0x00
#define PACKET_FORMAT_MSGPACK  0x01
#define PACKET_FORMAT_TEXT     0x02
#define PACKET_FORMAT_DATA     0x03 // raw binary data
#define PACKET_FORMAT_VOIDSTAR 0x04 // binary morloc formatted data

#define PACKET_COMPRESSION_NONE  0x00 // uncompressed
#define PACKET_ENCRYPTION_NONE   0x00 // unencrypted

#define PACKET_STATUS_PASS  0x00
#define PACKET_STATUS_FAIL  0x01


struct Message {
  char* data;
  uint64_t length;
};

struct Header {
    char command[8];
    uint32_t offset; // this is a 4 byte int, so I should use a short
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

std::string show_hex(const char* input, int length) {
    std::stringstream ss;

    for(int i = 0; i < length; i++) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)(unsigned char)input[i];
        if (i < length - 1) {
            ss << " ";
        }

    }

    return ss.str();
}


// Interop /////////////

// Read n bytes as an int starting from position offset in a char array
uint64_t read_uint64(const char* bytes, size_t offset){
  uint64_t x = 0;
  for(size_t i = 0; i < 8; i++){
    uint64_t multiplier = 1;
    multiplier = multiplier << (8 * (8 - i - 1));
    x += static_cast<unsigned char>(bytes[i + offset]) * multiplier;
  }
  return x;
}

uint32_t read_uint32(const char* bytes, size_t offset){
  uint32_t x = 0;
  for(size_t i = 0; i < 4; i++){
    uint32_t multiplier = 1;
    multiplier = multiplier << (8 * (4 - i - 1));
    x += static_cast<unsigned char>(bytes[i + offset]) * multiplier;
  }
  return x;
}

void write_int32(char* data, uint32_t value){
    data[0] = (value >> 24) & 0xFF;
    data[1] = (value >> 16) & 0xFF;
    data[2] = (value >>  8) & 0xFF;
    data[3] = value & 0xFF;
}

void write_int64(char* data, uint64_t value){

    data[0] = (value >> 56) & 0xFF;
    data[1] = (value >> 48) & 0xFF;
    data[2] = (value >> 40) & 0xFF;
    data[3] = (value >> 32) & 0xFF;
    data[4] = (value >> 24) & 0xFF;
    data[5] = (value >> 16) & 0xFF;
    data[6] = (value >>  8) & 0xFF;
    data[7] = value & 0xFF;
}

Header read_header(const char* msg){

    std::string magic = show_hex(msg, 4);

    if( magic != "6d f8 07 07"){
      std::string errmsg = "Bad magic: " + magic;
      log_message(errmsg);
      throw std::runtime_error(errmsg);
    }

    Header header;

    for(size_t offset = 0; offset < 8; offset++){
      header.command[offset] = msg[12+offset];
    }

    header.offset = read_uint32(msg, 20);
    header.length = read_uint64(msg, 24);

    return header;
}

// char command[8];
// int offset; // this is a 4 byte int, so I should use a short
// int length;
void make_header(char* data, char cmd[8], int offset, int length){
    data[ 0] = 0x6d;
    data[ 1] = 0xF8;
    data[ 2] = 0x07;
    data[ 3] = 0x07;
    data[ 4] = 0x00; // plain
    data[ 5] = 0x00;
    data[ 6] = 0x00; // version
    data[ 7] = 0x00;
    data[ 8] = 0x00; // version_flavor
    data[ 9] = 0x00;
    data[10] = 0x00; // mode
    data[11] = 0x00;
    for(size_t i = 0; i < 8; i++){
      data[12 + i] = cmd[i];
    }
    write_int32(data + 20, offset);
    write_int64(data + 24, length);
}

Message make_data(const char* data, size_t length, char src, char fmt, char cmpr, char encr, char status){
  Message packet;
  packet.length = 32 + length;
  packet.data = (char*)malloc(packet.length * sizeof(char));

  char cmd[8];
  cmd[0] = PACKET_TYPE_DATA;
  cmd[1] = src;
  cmd[2] = fmt;
  cmd[3] = cmpr;
  cmd[4] = encr;
  cmd[5] = status;
  cmd[6] = 0x00;
  cmd[7] = 0x00;

  // generate the header
  make_header(packet.data, cmd, 0, length);

  // directly copy the value data
  memcpy(packet.data + 32, data, length);

  return packet;
}

Message fail_packet(const std::string& errmsg){
  return make_data(
    errmsg.c_str(),
    errmsg.size(),
    PACKET_SOURCE_MESG,
    PACKET_FORMAT_TEXT,
    PACKET_COMPRESSION_NONE,
    PACKET_ENCRYPTION_NONE,
    PACKET_STATUS_FAIL
  );
}

std::string read(const std::string& file) {
    std::ifstream input_file(file);

    if (!input_file.is_open()) {
        throw std::runtime_error("Error opening file: " + file);
    }

    std::stringstream buffer;
    buffer << input_file.rdbuf();

    std::string content = buffer.str();

    return content;
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

    int length;

    Message packet;

    std::vector<char> payload = mpk_pack(value, schema_str);

    if (payload.size() <= 65536 - 32) {
        // for small data, send directly over the socket
        packet = make_data(
          payload.data(),
          payload.size(),
          PACKET_SOURCE_MESG,
          PACKET_FORMAT_MSGPACK,
          PACKET_COMPRESSION_NONE,
          PACKET_ENCRYPTION_NONE,
          PACKET_STATUS_PASS
        );
    } else {

        // for large data, write a temporary file
        std::string tmpfilename = generateTempFilename();
        std::ofstream tempFile(tmpfilename, std::ios::binary);
        if (!tempFile) {
            throw std::runtime_error("Failed to open temporary file: " + tmpfilename);
        }
        tempFile.write(payload.data(), payload.size());
        tempFile.close();

        packet = make_data(
          tmpfilename.data(),
          tmpfilename.size(),
          PACKET_SOURCE_FILE,
          PACKET_FORMAT_MSGPACK,
          PACKET_COMPRESSION_NONE,
          PACKET_ENCRYPTION_NONE,
          PACKET_STATUS_PASS
        );
    }

    log_message("Putting packet of length " + std::to_string(packet.length));

    return packet;
}


// Use a key to retrieve a value
template <typename T>
T _get_value(const Message& packet, const std::string& schema_str){
    Header header = read_header(packet.data);

    char source = header.command[1];
    char format = header.command[2];

    std::string errmsg = "";

    switch(source){
      case PACKET_SOURCE_MESG:
        switch(format){
          case PACKET_FORMAT_MSGPACK:
            std::vector<char> msg(packet.data + 32, packet.data + packet.length);
            return mpk_unpack<T>(msg, schema_str);
        }
        log_message("Invalid format");
        break;
      case PACKET_SOURCE_FILE:
        switch(format){
            case PACKET_FORMAT_MSGPACK:
                std::string filename(packet.data + 32, packet.data + packet.length);
                log_message("Reading filename " + filename + " of length " + std::to_string(packet.length));
                // read filename in as a std::vector<char> binary object named "msg"
                std::vector<char> msg;
                {
                    std::ifstream file(filename, std::ios::binary | std::ios::ate);
                    if (!file.is_open()) {
                        throw std::runtime_error("Unable to open file: " + filename);
                    }
                    std::streamsize size = file.tellg();
                    file.seekg(0, std::ios::beg);
          
                    msg.resize(size);
                    if (!file.read(msg.data(), size)) {
                        throw std::runtime_error("Failed to read file: " + filename);
                    }
                }
                return mpk_unpack<T>(msg, schema_str);
        }
        errmsg = "Invalid format";
        break;
      case PACKET_SOURCE_NXDB:
        errmsg = "Not yet supported";
        break;
      default:
        errmsg = "Invalid source";
        break;
    }

    throw std::runtime_error(errmsg);
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
    Header header = read_header(buffer);
    result.length = 32 + header.offset + header.length;

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

    log_message("Starting foreign call");

    Message call_packet;
    call_packet.length = 32;

    for(size_t i = 0; i < arg_keys.size(); i++){
      call_packet.length += arg_keys[i].length;
    }

    call_packet.data = (char*)malloc(call_packet.length * sizeof(char));

    char cmd[8];
    cmd[0] = PACKET_TYPE_CALL;
    write_int32(cmd + 1, mid);
    cmd[5] = 0x00;
    cmd[6] = 0x00;
    cmd[7] = 0x00;
    make_header(call_packet.data, cmd, 0, call_packet.length - 32);

    size_t arg_start = 32;
    for(size_t i = 0; i < arg_keys.size(); i++){
      Message arg = arg_keys[i];
      memcpy(call_packet.data + arg_start, arg.data, arg.length);
      arg_start += arg.length;
    }

    log_message("Send request to " + socket_path);
    struct Message result_msg = ask(socket_path.c_str(), call_packet);
    log_message("Receive data from cmd: " + socket_path);

    return result_msg;
}



// AUTO signatures statements start
// <<<BREAK>>>
// AUTO signatures statements end



// AUTO manifolds statements start
// <<<BREAK>>>
// AUTO manifolds statements end



Message dispatch(const Message& msg){

    Header header = read_header(msg.data);

    if (header.command[0] == PACKET_TYPE_PING){
        return msg;

    } else if (header.command[0] == PACKET_TYPE_CALL) {

      uint32_t mid = read_uint32(header.command, 1);

      log_message("dispatching to mid " + std::to_string(mid));

      Header arg_header;
      size_t pos = 32 + header.offset;
      size_t n = 0;
      while(pos < msg.length){
          arg_header = read_header(msg.data + pos);
          pos += 32 + arg_header.offset + arg_header.length;
          n++;
      }

      std::vector<Message> args(n);
      pos = 32 + header.offset;
      for(size_t i = 0; i < args.size(); i++){
          arg_header = read_header(msg.data + pos);
          args[i].length = 32 + arg_header.offset + arg_header.length;
          args[i].data = (char*)malloc(args[i].length * sizeof(char));
          memcpy(args[i].data, msg.data + pos, args[i].length);
          pos += 32 + arg_header.offset + arg_header.length;
          log_message("arg[" + std::to_string(i) + "] = " + show_hex(args[i].data, args[i].length));
      }



// AUTO dispatch statements start
// <<<BREAK>>>
// AUTO dispatch statements end

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
    if (argc != 3) {
        std::cerr << "Usage: " << argv[0] << " <socket_path>" << " <tmpdir>\n";
        return 1;
    }
    const char* socket_path = argv[1];

    // give a value to the global variable storing the main temporary directory
    g_tmpdir = std::string(argv[2]);

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
