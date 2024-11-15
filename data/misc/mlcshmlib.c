#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

void shinit(const char* shm_name, size_t shm_size);
void shmalloc(size_t size);
void shfree(void* ptr);
void shcalloc(size_t nmemb, size_t size);
void shrealloc(void* ptr, size_t size);
void shclean();

typedef struct shm {
  // A global lock that allows many readers but only one writer at a time
  pthread_rwlock_t* rwlock;

  // Pointer to the first memory block header
  block_header* root;
} shm;

typedef struct block_header {
    size_t size;
    int is_free;
    struct block_header *next;
} block_header;



static void *shm_base = NULL;
static block_header *head = NULL;

void shinit(const char* shm_name, size_t shm_size) {
    int fd;
    struct stat sb;
    int created = 0;
    shm* shared_mem;

    // Try to open existing shared memory object
    fd = shm_open(shm_name, O_RDWR, 0666);
    if (fd == -1) {
        if (errno == ENOENT) {
            // Shared memory object doesn't exist, create it
            fd = shm_open(shm_name, O_CREAT | O_RDWR, 0666);
            if (fd == -1) {
                perror("shm_open (create)");
                exit(1);
            }
            created = 1;
        } else {
            perror("shm_open (open)");
            exit(1);
        }
    }

    if (created) {
        // If we created the object, set its size
        if (ftruncate(fd, shm_size) == -1) {
            perror("ftruncate");
            exit(1);
        }
    } else {
        // Get the size of the shared memory object
        if (fstat(fd, &sb) == -1) {
            perror("fstat");
            exit(1);
        }
        // If we're loading an existing object, use its current size
        shm_size = sb.st_size;
    }

    // Map the shared memory object
    shm_base = mmap(NULL, shm_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (shm_base == MAP_FAILED) {
        perror("mmap");
        exit(1);
    }

    // Set up the shm struct at the beginning of the shared memory
    shared_mem = (shm*)shm_base;

    if (created) {
        // Initialize the read-write lock
        shared_mem->rwlock = (pthread_rwlock_t*)(shared_mem + 1);
        if (pthread_rwlock_init(shared_mem->rwlock, NULL) != 0) {
            perror("pthread_rwlock_init");
            exit(1);
        }

        // Initialize the first block
        shared_mem->root = (block_header*)((char*)shared_mem->rwlock + sizeof(pthread_rwlock_t));
        shared_mem->root->size = shm_size - sizeof(shm) - sizeof(pthread_rwlock_t) - sizeof(block_header);
        shared_mem->root->is_free = 1;
        shared_mem->root->next = NULL;
    } else {
        // If loading existing memory, just set the pointers
        shared_mem->rwlock = (pthread_rwlock_t*)(shared_mem + 1);
        shared_mem->root = (block_header*)((char*)shared_mem->rwlock + sizeof(pthread_rwlock_t));
    }

    // Update the global head pointer
    head = shared_mem->root;

    // Close the file descriptor (the mapping remains valid)
    close(fd);
}


// Helper function to find a suitable free block
static block_header* find_free_block(block_header** last, size_t size) {
    block_header* current = head;
    while (current && !(current->is_free && current->size >= size)) {
        *last = current;
        current = current->next;
    }
    return current;
}

// Helper function to split a block if it's too large
static void split_block(block_header* block, size_t size) {
    block_header* new_block;
    if (block->size > size + sizeof(block_header)) {
        new_block = (block_header*)((char*)block + size + sizeof(block_header));
        new_block->size = block->size - size - sizeof(block_header);
        new_block->is_free = 1;
        new_block->next = block->next;
        block->size = size;
        block->next = new_block;
    }
}


void* shmalloc(size_t size) {
    shm* shared_mem = (shm*)shm_base;
    block_header* block, * last;
    size_t total_size;
    void* result;

    if (size == 0)
        return NULL;

    pthread_rwlock_wrlock(shared_mem->rwlock);

    block = find_free_block(&last, size);
    if (block) {
        // If a suitable block is found
        block->is_free = 0;
        split_block(block, size);
        result = (void*)(block + 1);
    } else {
        // No suitable block found
        result = NULL;
    }

    pthread_rwlock_unlock(shared_mem->rwlock);

    return result;
}


void shfree(void* ptr) {
    if (!ptr)
        return;

    shm* shared_mem = (shm*)shm_base;
    block_header* header;
    
    pthread_rwlock_wrlock(shared_mem->rwlock);

    header = (block_header*)ptr - 1;
    header->is_free = 1;

    // Merge with next block if it's free
    if (header->next && header->next->is_free) {
        header->size += sizeof(block_header) + header->next->size;
        header->next = header->next->next;
    }

    pthread_rwlock_unlock(shared_mem->rwlock);
}


void* shcalloc(size_t nmemb, size_t size) {
    size_t total_size;
    void* ptr;

    total_size = nmemb * size;
    if (nmemb != 0 && total_size / nmemb != size)
        return NULL; // Check for overflow

    ptr = shmalloc(total_size);
    if (ptr)
        memset(ptr, 0, total_size);

    return ptr;
}


void* shrealloc(void* ptr, size_t size) {
    shm* shared_mem = (shm*)shm_base;
    block_header* header;
    void* new_ptr;

    if (!ptr)
        return shmalloc(size);

    if (size == 0) {
        shfree(ptr);
        return NULL;
    }

    pthread_rwlock_wrlock(shared_mem->rwlock);

    header = (block_header*)ptr - 1;
    
    if (header->size >= size) {
        // The current block is large enough
        split_block(header, size);
        new_ptr = ptr;
    } else {
        // Need to allocate a new block
        pthread_rwlock_unlock(shared_mem->rwlock);
        new_ptr = shmalloc(size);
        if (new_ptr) {
            memcpy(new_ptr, ptr, header->size);
            shfree(ptr);
        }
        return new_ptr;
    }

    pthread_rwlock_unlock(shared_mem->rwlock);

    return new_ptr;
}


void shclean(){
    shm* shared_mem = (shm*)shm_base;
    block_header* header = shared_mem->root;

    pthread_rwlock_wrlock(shared_mem->rwlock);

    while(header->next){
      if(header->is_free && header->next->is_free){
        header->size += sizeof(block_header) + header->next->size;
        header->next = header->next->next;
      } else {
        header = header->next;
      }
    }

    pthread_rwlock_unlock(shared_mem->rwlock);
}
