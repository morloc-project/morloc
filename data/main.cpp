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
            log_message("Job dispatched");

            // return the result to the client
            send(new_job.client_fd, result.data, result.length, 0);

            // close the current client
            close(new_job.client_fd);

            log_message("Job finished, connection closed");

            // And exit the child
            exit(0);
        } else if (pid > 0) {
            // Entered by the parent process
            new_job.pid = pid;
            children.push_back(new_job);
            log_message("Parent process: added child with PID " + std::to_string(pid));
        } else {
            log_message("Fork failed");
            close(client_fd);
        }
    } else {
        // Close the connection if no data was received
        log_message("No data received, closing connection");
        close(client_fd);
    }
}


int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " <socket_path>\n";
        return 1;
    }
    const char* SOCKET_PATH = argv[1];

    log_message("Server starting with socket path: " + std::string(SOCKET_PATH));

    // Setup a new server that uses a given path for the socket address
    int server_fd = new_server(SOCKET_PATH);

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
    close(server_fd);
    log_message("Server socket closed");

    // Remove the socket file
    unlink(SOCKET_PATH);
    log_message("Socket file removed");

    return 0;
}
