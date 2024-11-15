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


#define PACKET_TYPE_DATA  0x00
#define PACKET_TYPE_CALL  0x01
#define PACKET_TYPE_PING  0x02
#define PACKET_TYPE_GET   0x03
#define PACKET_TYPE_PUT   0x04
#define PACKET_TYPE_DEL   0x05

#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_SMEM  0x02 // the message is an index in a shared memory pool

#define PACKET_FORMAT_JSON     0x00
#define PACKET_FORMAT_MSGPACK  0x01
#define PACKET_FORMAT_TEXT     0x02

#define PACKET_COMPRESSION_NONE  0x00 // uncompressed
#define PACKET_ENCRYPTION_NONE   0x00 // unencrypted

#define PACKET_STATUS_PASS  0x00
#define PACKET_STATUS_FAIL  0x01


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

  // The shared memory file descriptor
  int fd;

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
  size_t next_block_idx;

  // The offset of the next block relative to the start of the memory pool
  // (it will range from addr_size * sizeof(Addr) up to pool_size - 1)
  size_t next_block_loc;
  
} MemoryPool;

typedef struct Op{
  char method;
  // either a key to delete or the size of the desired memory block
  size_t mesg;
} Op;

void resize_memory_pool(MemoryPool* mempool, size_t new_size){

    // Set size of shared memory object
    if (ftruncate(mempool->fd, new_size) == -1) {
        perror("ftruncate");
        exit(1);
    }

    // Free old memory map
    munmap(mempool->data);

    // Map shared memory object into process address space
    mempool->data = mmap(NULL, new_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (mempool->data == MAP_FAILED) {
        perror("mmap");
        exit(1);
    }
}


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

void write_uint64(char* data, uint64_t value){
    data[0] = (value >> 56) & 0xFF;
    data[1] = (value >> 48) & 0xFF;
    data[2] = (value >> 40) & 0xFF;
    data[3] = (value >> 32) & 0xFF;
    data[4] = (value >> 24) & 0xFF;
    data[5] = (value >> 16) & 0xFF;
    data[6] = (value >>  8) & 0xFF;
    data[7] = value & 0xFF;
}

Op read_header(const char* buffer){

    Op op;
    op.method = char[12];

    if(! (
      buffer[0] == 0x6d && 
      buffer[1] == 0xf8 && 
      buffer[2] == 0x07 && 
      buffer[3] == 0x07 && 
      (op.method == PACKET_TYPE_DEL || method == PACKET_TYPE_GET)
    )){
      op.mesg = 0;
    } else {
      op.mesg = read_uint64(buffer, 32);
    }

    return op;
}


size_t write_key(Key key, char* buffer){
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
  // command
  data[12] = PACKET_TYPE_DATA;
  data[13] = PACKET_FORMAT_DATA;
  data[14] = PACKET_COMPRESSION_NONE;
  data[15] = PACKET_ENCRYPTION_NONE;
  data[16] = PACKET_STATUS_PASS;
  data[17] = 0x00;
  data[18] = 0x00;
  data[19] = 0x00;
  // offset
  write_uint32(data + 20, 0);
  // length, 2 X 64bit integers
  write_int64(data + 24, 16);
  // message
  write_int64(data + 32, key.index);
  write_int64(data + 40, key.pool_size);

  return 32 + 16;
}

void defragment(MemoryPool* mempool){

}

void resize(size_t required_size, MemoryPool* mempool){

}

Key get_block(size_t size, MemoryPool* mempool){

  if(mempool->size - mempool->next_block_loc < size){
    defragment(mempool);  
  }

  if(mempool->size - mempool->next_block_loc < size){
    resize(mempool);
  }

  Block block;
  block.size = ((size + PAGE_SIZE - 1) / PAGE_SIZE) * PAGE_SIZE;

  block.index =

  mempool->blocks[mempool->next_block_idx] = 


  // if there is not enough space for the desired block, defragment
  // if there is still not enough space, resize
  // create a new block, make an index and return
}


void run_job(int fd, char* buffer, MemoryPool* mempool) {
    size_t reply_length;
    ssize_t recv_length;
    ssize_t bytes_sent;
    Op op;
    Key key;

    // read from unix fomain socket on file descriptor fd
    recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);

    op = read_header(buffer);

    switch(op.method){
      case PACKET_TYPE_GET:
        key = get_block(op.mesg, mempool); 
        reply_length = write_key(key, buffer);
        bytes_sent = send(fd, buffer, reply_length, 0); 
        if(bytes_sent != reply_length){
          perror("Failed to send reply");
        }
        break;
      case PACKET_TYPE_DEL:
        // Indicate that the index is free
        // The memory will be freed for reuse with the next defragmentation call
        mempool->data[op.mesg] = 0; 
        break;
      default:
        perror("Bad packet");
        break;
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
    char* message_buffer = (char*)malloc(BUFFER_SIZE * sizeof(char));
    

    // Check if we have at least 3 command-line arguments
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
    mempool.shm_name       = shm_name;
    mempool.data           = NULL;
    mempool.pool_size      = initial_size;
    mempool.blocks         = NULL;
    mempool.addr_size      = 1024 * sizeof(Addr);
    mempool.next_block_idx = 0;
    mempool.next_block_loc = mempool.addr_size;

    // Open shared memory object
    mempool.fd = shm_open(shm_name, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
    if (mempool.fd == -1) {
        perror("shm_open");
        exit(1);
    }

    // set the initial size of the memory pool
    resize_memory_pool(&mempool, initial_size);



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
                run_job(client_fds[i], message_buffer, &mempool);
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
    free(message_buffer);

    return 0;
}
