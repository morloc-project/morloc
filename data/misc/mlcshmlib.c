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

#define SHM_MAGIC 0xF00DCAFE
#define BLK_MAGIC 0xF00DB10C
#define MAX_FILENAME_SIZE 128
#define MAX_VOLUME_NUMBER 32

// An index into a multi-volume shared memory pool
typedef relptr_t size_t; 

// An index into a single volume. 0 is the start of the first block immediately
// following the shm object.
typedef volptr_t size_t; 

// An absolute pointer to system memory
typedef absptr_t void*; 

typedef struct shm {
  // A constant identifying this as a morloc shared memory file
  int magic;

  // The name of this volume. Used for creating debugging messages.
  char[MAX_FILENAME_SIZE] volume_name;

  // A memory pool will consist of one or more volumes. They all share the same
  // base name (volume_name) followed by an underscore and their index. E.g.,
  // `${TMPDIR}/morloc_shm_0`, `${TMPDIR}/morloc_shm_1`, etc.
  int volume_index;

  // The number of bytes that can be stored in this volume. This number will be
  // used to calculate relative offsets. Pools may pass relative pointers shared
  // objects. The pointer will first be checked against the first shared memory
  // volume. If this volume is smaller than the pointer, volume_size will be
  // subtracted from the pointer and it will be checked against the next volume.
  size_t volume_size;

  // There may be many shared memory volumes. If this is the first volume, its
  // relative offset will be 0. Otherwise, it will be the sum of prior volume
  // sizes. Clients do not know the size of the volume or the number of
  // volumes. From their point of view, there is one infinite memory pool and
  // the relative pointers they use point to positions in that pool.
  size_t relative_offset;

  // A global lock that allows many readers but only one writer at a time
  pthread_rwlock_t rwlock;

  // Pointer to the current free memory block header
  volptr_t cursor;
} shm_t;

typedef struct block_header {
    int magic
    int reference_count;
    size_t size;
} block_header_t;


// The index of the current volum
static size_t current_volume = 0;

static void* volumes[MAX_VOLUME_NUMBER];
for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++){
  volumes[i] = NULL;
}


volptr_t rel2vol(relptr_t ptr){
  for(size_t i = 0; i < MAX_VOLUME_NUMBER; i++){
    (shm_t*) shm = (shm_t*)volumes[i];
    if(shm){
      if(ptr < volume_size){
        return ptr;
      } else {
        ptr -= shm->volume_size;
      }
    } else {
      perror("No memory pool found\n");
    }
  }
  perror("No memory pool found\n");
}

absptr_t rel2abs(relptr_t ptr){
  for(size_t i = 0; i < MAX_VOLUME_NUMBER; i++){
    (shm_t*) shm = (shm_t*)volumes[i];
    if(shm){
      if(ptr < volume_size){
        return ptr + shm + sizeof(shm_t);
      } else {
        ptr -= shm->volume_size;
      }
    } else {
      perror("No memory pool found\n");
    }
  }
  perror("No memory pool found\n");
}

relptr_t vol2rel(volptr_t ptr, shm_t shm){
  return ((shm_t*)(shm_))->relative_offset + ptr;
}

absptr_t vol2abs(volptr_t ptr){
  return shm_base + sizeof(shm_t) + ptr;
}

volptr_t abs2vol(){
  return ptr - shm_base - sizeof(shm_t);
}

relptr_t abs2rel(absptr_t ptr){
  return ptr - shm_base - sizeof(shm_t) + ((shm_t*)(shm_base))->relative_offset;
}



void shinit(const char* shm_name, size_t shm_size) {
    
    int fd;
    struct stat sb;
    int created = 0;
    shm_t* shared_mem;

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
    shared_mem = (shm_t*)shm_base;

    if (created) {
        shared_mem->magic = SHM_MAGIC;
        // Initialize the read-write lock
        if (pthread_rwlock_init(shared_mem->rwlock, NULL) != 0) {
            perror("pthread_rwlock_init");
            exit(1);
        }

        // Initialize the first block
        shared_mem->root = (block_header_t*)((char*)shared_mem + sizeof(shm_t));
        shared_mem->root->size = shm_size - sizeof(shm_t) - sizeof(block_header_t);
        shared_mem->root->is_free = 1;
        shared_mem->root->next = NULL;
    } else {
        // If loading existing memory, just set the pointers
        shared_mem->rwlock = (pthread_rwlock_t*)(shared_mem + 1);
        shared_mem->root = (block_header_t*)((char*)shared_mem + sizeof(shm_t));
    }

    // Update the global head pointer
    head = shared_mem->root;

    // Close the file descriptor (the mapping remains valid)
    close(fd);
}


// Helper function to find a suitable free block
static block_header_t* find_free_block(block_header_t** last, size_t size) {
    block_header_t* current = head;
    while (current && !(current->is_free && current->size >= size)) {
        *last = current;
        current = current->next;
    }
    return current;
}

// Helper function to split a block if it's too large
static void split_block(block_header_t* block, size_t size) {
    block_header_t* new_block;
    if (block->size > size + sizeof(block_header_t)) {
        new_block = (bloc_header_t*)((char*)block + size + sizeof(bloc_header_t));
        new_block->size = block->size - size - sizeof(bloc_header_t);
        new_block->is_free = 1;
        new_block->next = block->next;
        block->size = size;
        block->next = new_block;
    }
}


void* shmalloc(size_t size) {
    shm_t* shared_mem = (shm_t*)shm_base;
    bloc_header_t* block, * last;
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

    shm_t* shared_mem = (shm_t*)shm_base;
    bloc_header_t* header;
    
    pthread_rwlock_wrlock(shared_mem->rwlock);

    header = (bloc_header_t*)ptr - 1;
    header->is_free = 1;

    // Merge with next block if it's free
    if (header->next && header->next->is_free) {
        header->size += sizeof(bloc_header_t) + header->next->size;
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
    shm_t* shared_mem = (shm_t*)shm_base;
    bloc_header_t* header;
    void* new_ptr;

    if (!ptr)
        return shmalloc(size);

    if (size == 0) {
        shfree(ptr);
        return NULL;
    }

    pthread_rwlock_wrlock(shared_mem->rwlock);

    header = (bloc_header_t*)ptr - 1;
    
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
    shm_t* shared_mem = (shm_t*)shm_base;
    bloc_header_t* header = shared_mem->root;

    pthread_rwlock_wrlock(shared_mem->rwlock);

    while(header->next){
      if(header->is_free && header->next->is_free){
        header->size += sizeof(bloc_header_t) + header->next->size;
        header->next = header->next->next;
      } else {
        header = header->next;
      }
    }

    pthread_rwlock_unlock(shared_mem->rwlock);
}
