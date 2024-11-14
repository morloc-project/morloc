#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#define MAX_CLIENTS 48
#define BUFFER_SIZE 1024

#define PAGE_SIZE 0x20000

// Stores information on one contiguous block of memory
typedef struct Block{
  // the address array index (0 indicates that the block is free)
  size_t index;

  // size of the block in pages of length PAGE_SIZE
  size_t size;
} Block;


// A Key is passed to a client with the info needed to access a block of memory
typedef struct Key {

  // an index ranging from 1 to addr_size
  // an index of 0 indicates that the key is has been freed
  size_t index; 

  // the size of the memory pool, when this changes, the client must re-mmap
  size_t pool_size; 
} Key ;


// Addr structures are written to the first MemoryPool.addr_size positions
// in the shared memory block. Clients may access these positions to find
// their data blocks.
typedef struct Addr{

  // Offset of the data block relative to the start of the memory pool
  size_t offset;

  // The size of the data stored in the block (NOT the size of the block, which
  // will be some multiple of the PAGE_SIZE).
  size_t size; 
} Addr;


typedef struct MemoryPool{

  // The name of the shared memory pool, e.g., "/morloc-memory-pool-1"
  char* shm_name;

  // A pointer to the shared memory map for this process
  void* data;

  // The size of the pool in bytes (this should be a multiple of PAGE_SIZE)
  size_t pool_size;

  // The number of addresses that may be stored at the beginning of the
  // pool. The total size of this block will be addr_size * sizeof(Addr).
  size_t addr_size;

  // A vector of allocated blocks
  Block* blocks;

  // The lowest unassigned block in `blocks`
  size_t next_block;
  
} MemoryPool;


void resize_addr(MemoryPool mempool, size_t size){
    if (mempool.blocks == NULL){
      free(mempool.blocks);
    }

    mempool.addr_size = size;
    mempool.blocks = (Block*)calloc(size, sizeof(Block));

    if (mempool.blocks == NULL) {
      perror("malloc");
      exit(1);
    }
}

// Stub function for run_job
void run_job(int fd) {
    char buffer[BUFFER_SIZE];
    ssize_t bytes_read = read(fd, buffer, BUFFER_SIZE - 1);
    if (bytes_read > 0) {
        buffer[bytes_read] = '\0';
        printf("Received from client: %s\n", buffer);
       // Process the job here
    } else if (bytes_read == 0) {
        printf("Client disconnected\n");
    } else {
        perror("Error reading from client");
    }
}

// Function to close a socket and log the action
void socket_close(int fd, const char* context) {
    close(fd);
    char message[100];
    snprintf(message, sizeof(message), "Closed socket for %s", context);
    perror(message);
}

int main(int argc, char * argv[]){

    size_t initial_size;
    char* shm_name;
    int fd;

    // Check if we have at least 2 command-line arguments
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <shared_memory_name> <initial_size> <socket_path>\n", argv[0]);
        exit(1);
    }

    // Set shm_name to the first argument
    shm_name = argv[1];

    // Convert the second argument to a size_t for initial_size
    char *endptr;
    initial_size = strtoull(argv[2], &endptr, 10);
    // Check for conversion errors
    if (*endptr != '\0' || errno == ERANGE) {
        fprintf(stderr, "Invalid size argument: %s\n", argv[2]);
        exit(1);
    }
    // Round up initial_size to the nearest multiple of PAGE_SIZE
    initial_size = ((initial_size + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE;

    char* socket_path = argv[3];

    MemoryPool mempool;
    mempool.shm_name   = shm_name;
    mempool.data       = NULL;
    mempool.pool_size  = initial_size;
    mempool.blocks     = NULL;
    mempool.next_block = 0;

    // Open shared memory object
    fd = shm_open(shm_name, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
    if (fd == -1) {
        perror("shm_open");
        exit(1);
    }

    // Set size of shared memory object
    if (ftruncate(fd, initial_size) == -1) {
        perror("ftruncate");
        exit(1);
    }

    // Map shared memory object into process address space
    mempool.data = mmap(NULL, initial_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (mempool.data == MAP_FAILED) {
        perror("mmap");
        exit(1);
    }

    resize_addr(mempool, 1024); 



    // --------------------------------------------------------------



    int server_fd, max_fd, ready, client_fd;
    struct sockaddr_un server_addr;
    fd_set read_fds;
    struct timeval tv;
    int client_fds[MAX_CLIENTS] = {0};  // Array to store client file descriptors
    int client_count = 0;  // Keep track of number of connected clients

    // Create a UNIX domain socket for local inter-process communication
    server_fd = socket(AF_UNIX, SOCK_STREAM, 0);
    if (server_fd == -1) {
        perror("Error creating socket");
        return 1;
    }

    // Initialize server address structure
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);

    // Remove any existing socket file
    unlink(socket_path);

    // Bind the socket to the address
    if (bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == -1) {
        perror("Error binding socket");
        socket_close(server_fd, "failed server");
        return 1;
    }

    // Start listening for incoming connections
    if (listen(server_fd, 1) == -1) {
        perror("Error listening on socket");
        socket_close(server_fd, "failed server");
        return 1;
    }

    // Set the server socket to non-blocking mode
    fcntl(server_fd, F_SETFL, O_NONBLOCK);

    while (1) {
        // Initialize the file descriptor set for select()
        FD_ZERO(&read_fds);
        FD_SET(server_fd, &read_fds);
        max_fd = server_fd;

        // Add client sockets to the file descriptor set
        for (int i = 0; i < client_count; i++) {
            FD_SET(client_fds[i], &read_fds);
            if (client_fds[i] > max_fd) {
                max_fd = client_fds[i];
            }
        }

        // Set timeout for select() to 100 microseconds
        tv.tv_sec = 0;
        tv.tv_usec = 100;

        // Wait for activity on any of the sockets
        ready = select(max_fd + 1, &read_fds, NULL, NULL, &tv);

        if (ready == -1) {
            perror("Error in select");
            continue;
        }

        // Check if there's a new incoming connection
        if (FD_ISSET(server_fd, &read_fds)) {
            client_fd = accept(server_fd, NULL, NULL);
            if (client_fd > 0) {
                printf("Accepted new client connection\n");
                fcntl(client_fd, F_SETFL, O_NONBLOCK);
                if (client_count < MAX_CLIENTS) {
                    client_fds[client_count++] = client_fd;
                } else {
                    perror("Max clients reached. Closing new connection.");
                    close(client_fd);
                }
            } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
                perror("Error accepting client connection");
            }
        }

        // Check for activity on client sockets
        for (int i = 0; i < client_count; i++) {
            if (FD_ISSET(client_fds[i], &read_fds)) {
                run_job(client_fds[i]);
                // Remove the client from the array and close the socket
                close(client_fds[i]);
                for (int j = i; j < client_count - 1; j++) {
                    client_fds[j] = client_fds[j + 1];
                }
                client_count--;
                i--;  // Adjust the loop counter as we've removed an element
            }
        }
    }

    // Close the server socket
    socket_close(server_fd, "server");

    // Remove the socket file
    unlink(socket_path);



    // --------------------------------------------------------------



    // Unmap the shared memory
    if (munmap(mempool.data, initial_size) == -1) {
        perror("munmap");
        exit(1);
    }

    shm_unlink(shm_name);

    // Close the file descriptor
    if (close(fd) == -1) {
        perror("close");
        exit(1);
    }

    free(mempool.blocks);

    return 0;
}
