// Interop /////////////

// Read n bytes as an int starting from position offset in a char array
uint64_t read_uint64(const char* bytes, size_t offset){
  uint64_t x = 0;
  for(size_t i = 0; i < 8; i++){
    uint64_t multiplier = 1;
    multiplier = multiplier << (8 * (8 - i - 1));
    x += static_cast<unsigned char>(bytes[i + offset]) * multiplier;
  }

  log_message("read uint64 " + std::to_string(x) + " from bytes " + show_hex(bytes + offset, 8));

  return x;
}

uint32_t read_uint32(const char* bytes, size_t offset){
  uint32_t x = 0;
  for(size_t i = 0; i < 4; i++){
    uint32_t multiplier = 1;
    multiplier = multiplier << (8 * (4 - i - 1));
    x += static_cast<unsigned char>(bytes[i + offset]) * multiplier;
  }

  log_message("read uint32 " + std::to_string(x) + " from bytes " + show_hex(bytes + offset, 4));

  return x;
}

void write_int32(char* data, uint32_t value){

    data[0] = (value >> 24) & 0xFF;
    data[1] = (value >> 16) & 0xFF;
    data[2] = (value >>  8) & 0xFF;
    data[3] = value & 0xFF;

    log_message("write_int32 convert " + std::to_string(value) + "(" + std::to_string(sizeof(value)) + ") -> " + show_hex(data, 4));
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

    log_message("write_int64 convert " + std::to_string(value) + "(" + std::to_string(sizeof(value)) + ") -> " + show_hex(data, 8));
}

Header read_header(const char* msg){

    std::string magic = show_hex(msg, 4);

    if( magic != "6d f8 07 07"){
      std::string errmsg = "Bad magic: " + magic;
      log_message(errmsg);
      throw std::runtime_error(errmsg);
    }

    log_message("reading header: " + show_hex(msg, 32));

    Header header;

    for(size_t offset = 0; offset < 8; offset++){
      header.command[offset] = msg[12+offset];
    }

    header.offset = read_uint32(msg, 20);
    header.length = read_uint64(msg, 24);

    log_message("header.offset: " + std::to_string(header.offset));
    log_message("header.length: " + std::to_string(header.length));

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
    PACKET_FORMAT_JSON,
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
    char template_file[] = "/tmp/morloc_cpp_XXXXXX";
    int fd = mkstemp(template_file);

    if (fd == -1) {
        perror("Error generating temporary filename");
        exit(EXIT_FAILURE);
    }

    // Close the file descriptor
    close(fd);

    return std::string(template_file);
}

// Transforms a serialized value into a message ready for the socket
Message _put_value(const std::string& value) {

    int length;

    Message packet;

    if (value.size() <= 65536 - 32) {
        // for small data, send directly over the socket
        packet = make_data(
          value.c_str(),
          value.size(),
          PACKET_SOURCE_MESG,
          PACKET_FORMAT_JSON,
          PACKET_COMPRESSION_NONE,
          PACKET_ENCRYPTION_NONE,
          PACKET_STATUS_PASS
        );
    } else {
        // for large data, write a temporary file

        std::string tmpfilename = generateTempFilename();
        std::ofstream tempFile(tmpfilename);
        tempFile << value;
        tempFile.close();

        packet = make_data(
          tmpfilename.c_str(),
          tmpfilename.size(),
          PACKET_SOURCE_FILE,
          PACKET_FORMAT_JSON,
          PACKET_COMPRESSION_NONE,
          PACKET_ENCRYPTION_NONE,
          PACKET_STATUS_PASS
        );
    }

    log_message("Putting packet of length " + std::to_string(packet.length));

    return packet;
}


// Use a key to retrieve a value
std::string _get_value(const Message& packet){
    Header header = read_header(packet.data);

    char source = header.command[1];
    char format = header.command[2];

    std::string errmsg = "";

    switch(source){
      case PACKET_SOURCE_MESG:
        switch(format){
          case PACKET_FORMAT_JSON:
            std::string msg(packet.data + 32, packet.data + packet.length);
            return msg;
        }
        log_message("Invalid format");
        break;
      case PACKET_SOURCE_FILE:
        switch(format){
          case PACKET_FORMAT_JSON:
            std::string filename(packet.data + 32, packet.data + packet.length);
            log_message("Reading filename " + filename + " of length " + std::to_string(packet.length));
            return read(filename);
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

struct sockaddr_un new_server_addr(const char* SOCKET_PATH){
    // Set up the server address structure
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, SOCKET_PATH, sizeof(server_addr.sun_path) - 1);
    return server_addr;
}

int new_server(const char* SOCKET_PATH){

    int server_fd = new_socket();

    struct sockaddr_un server_addr = new_server_addr(SOCKET_PATH);

    // Remove any existing socket file
    unlink(SOCKET_PATH);

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

int accept_client(int server_fd){
    // Accept a connection
    int client_fd = accept(server_fd, nullptr, nullptr);
    return client_fd;
}

int accept_client(int server_fd, double timeout_seconds) {
    fd_set readfds;
    struct timeval tv;
    int client_fd;

    // Clear the set
    FD_ZERO(&readfds);

    // Add server socket to the set
    FD_SET(server_fd, &readfds);

    // Set up the timeout
    tv.tv_sec = static_cast<long>(floor(timeout_seconds));
    tv.tv_usec = static_cast<long>((timeout_seconds - tv.tv_sec) * 1e6);

    // Wait for activity on the socket, with timeout
    int activity = select(server_fd + 1, &readfds, nullptr, nullptr, &tv);

    if (activity < 0) {
        // Error occurred
        return -1;
    } else if (activity == 0) {
        // Timeout occurred
        return -2;
    } else {
        // There is activity on the socket
        if (FD_ISSET(server_fd, &readfds)) {
            // Accept the connection
            client_fd = accept(server_fd, nullptr, nullptr);
            return client_fd;
        }
    }

    // This should not be reached, but just in case
    return -1;
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
