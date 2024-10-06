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
std::string _put_value(const std::string& value) {
    std::string packet(8, '\0');
    packet[0] = PACKET_HEAD;
    packet[1] = PACKET_VERSION;
    uint32_t length;

    if (value.size() <= 65536 - 8) {
        // for small data, send directly over the socket
        packet[2] = PACKET_SOURCE_MESG;
        packet[3] = PACKET_FORMAT_JSON;

        packet += value;
        length = static_cast<uint32_t>(value.size());
    } else {
        // for large data, write a temporary file
        packet[2] = PACKET_SOURCE_FILE;
        packet[3] = PACKET_FORMAT_JSON;

        std::string tmpfilename = generateTempFilename();
        std::ofstream tempFile(tmpfilename);
        tempFile << value;
        tempFile.close();
        packet += tmpfilename;
        length = static_cast<uint32_t>(tmpfilename.size());
    }

    // encode the length of the data
    packet[4] = (length >> 24) & 0xFF;
    packet[5] = (length >> 16) & 0xFF;
    packet[6] = (length >>  8) & 0xFF;
    packet[7] =  length        & 0xFF;

    return packet;
}


// Read 4 bytes as package length an int32
int readInt32(const std::string& bytes, size_t idx){
    size_t x = 0;
    x = (x << 8) + bytes[2];
    x = (x << 8) + bytes[3];
    x = (x << 8) + bytes[4];
    x = (x << 8) + bytes[5];
    return x;
}

// Use a key to retrieve a value
std::string _get_value(const std::string& data){
    char source = data[0];
    char format = data[1];
    int size = readInt32(data, 2);

    switch(source){
      case PACKET_SOURCE_MESG:
        switch(format){
          case PACKET_FORMAT_JSON:
            return data.substr(6);
          default:
            log_message("Invalid format");
        }
        break;
      case PACKET_SOURCE_FILE:
        switch(format){
          case PACKET_FORMAT_JSON:
            return read(data.substr(6));
          default:
            log_message("Invalid format");
        }
        break;
      case PACKET_SOURCE_NXDB:
        log_message("Not yet supported");
        break;
      default:
        log_message("Invalid source");
        break;
    }

    return ""; // fail
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
        std::cerr << "Error binding socket\n";
        close(server_fd);
        return 1;
    }

    // Listen for connections
    if (listen(server_fd, 1) == -1) {
        std::cerr << "Error listening on socket\n";
        close(server_fd);
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

Message ask(const char* socket_path, const Message& message){
    struct Message result;
    result.length = 0;

    int client_fd = new_socket();

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Connect to the server
    if (connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        close(client_fd);
        return result;
    }

    // Send a message
    send(client_fd, message.data, message.length, 0);

    // Receive response
    result.length = recv(client_fd, result.data, BUFFER_SIZE, 0);

    close(client_fd);

    return result;
}

std::string foreign_call(
    const std::string& socket_path,
    const std::string& cmd,
    const std::vector<std::string>& arg_keys
) {

    log_message("Starting foreign call");

    std::string full_cmd = cmd;
    for (const auto& arg : arg_keys) {
        full_cmd += " " + arg;
    }

    // Execute the command and capture the output
    struct Message message;
    message.length = full_cmd.size();
    memcpy(message.data, full_cmd.c_str(), BUFFER_SIZE);

    log_message("Send request to '" + socket_path + "' with cmd: " + full_cmd);
    struct Message result_msg = ask(socket_path.c_str(), message);
    log_message("Receive data from cmd: " + full_cmd);

    std::string result_str(result_msg.data, result_msg.length);

    return result_str;
}
