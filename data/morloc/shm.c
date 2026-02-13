#include "morloc.h"

// Global state
static _Thread_local size_t current_volume = 0;
static char common_basename[MAX_FILENAME_SIZE];
static shm_t* volumes[MAX_VOLUME_NUMBER] = {NULL};

// Protects all allocation/deallocation operations on the shared memory pool.
// Conversion functions (rel2abs, abs2rel, etc.) are safe without the mutex
// because volumes[] entries are written atomically and only transition from
// NULL to a valid pointer (monotonic creation).
static pthread_mutex_t alloc_mutex = PTHREAD_MUTEX_INITIALIZER;

volptr_t rel2vol(relptr_t ptr, ERRMSG) {
    VAL_RETURN_SETUP(volptr_t, VOLNULL)

    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = TRY(shopen, i);
        if (shm == NULL) {
            RAISE("Shared volume %zu does not exist", ptr)
        }
        if ((size_t)ptr < shm->volume_size) {
            return (volptr_t)ptr;
        }
        ptr -= (relptr_t)shm->volume_size;
    }

    RAISE("Failed to find volume for relative pointer %zu", ptr)
}

absptr_t rel2abs(relptr_t ptr, ERRMSG) {
    VAL_RETURN_SETUP(absptr_t, NULL)

    RAISE_IF(ptr < 0, "Illegal relptr_t value of %zu", ptr)

    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = TRY(shopen, i);
        if (shm == NULL) {
            RAISE("Failed to find shared volume %zu while searching for relative pointer %zu", i, ptr)
        }
        if ((size_t)ptr < shm->volume_size) {
            char* shm_start = (char*)shm;
            return (absptr_t)(shm_start + sizeof(shm_t) + ptr);
        }
        ptr -= (relptr_t)shm->volume_size;
    }

    RAISE("Shared memory pool does not contain index %zu", ptr)
}

relptr_t vol2rel(volptr_t ptr, shm_t* shm) {
    return (relptr_t)(shm->relative_offset + ptr);
}

absptr_t vol2abs(volptr_t ptr, shm_t* shm) {
    char* shm_start = (char*)shm;
    return (absptr_t)(shm_start + sizeof(shm_t) + ptr);
}

volptr_t abs2vol(absptr_t ptr, shm_t* shm, ERRMSG) {
    VAL_RETURN_SETUP(volptr_t, VOLNULL)

    // if a shared memory volume is given, search it
    if (shm) {
        char* shm_start = (char*)shm;
        char* data_start = shm_start + sizeof(shm_t);
        char* data_end = data_start + shm->volume_size;

        if ((char*)ptr >= data_start && (char*)ptr < data_end) {
            return (volptr_t)((char*)ptr - data_start);
        } else {
            RAISE("Could not find absolute pointer %p in shared memory pool", ptr)
        }
    // otherwise, seek a suitable volume
    } else {
        for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
            shm_t* current_shm = shopen(i, &CHILD_ERRMSG);
            FREE(CHILD_ERRMSG); // failure is OK
            if (current_shm != NULL) {
                char* shm_start = (char*)current_shm;
                char* data_start = shm_start + sizeof(shm_t);
                char* data_end = data_start + current_shm->volume_size;

                if ((char*)ptr >= data_start && (char*)ptr < data_end) {
                    return (volptr_t)((char*)ptr - data_start);
                }
            }
        }
    }
    return VOLNULL;
}

relptr_t abs2rel(absptr_t ptr, ERRMSG) {
    VAL_RETURN_SETUP(relptr_t, RELNULL)

    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = shopen(i, &CHILD_ERRMSG);
        FREE(CHILD_ERRMSG); // failure is OK, will continue searching
        if (shm == NULL) {
            continue;
        }
        char* shm_start = (char*)shm;
        char* data_start = shm_start + sizeof(shm_t);
        char* data_end = data_start + shm->volume_size;

        if ((char*)ptr > shm_start && (char*)ptr < data_end) {
            return (relptr_t)((char*)ptr - data_start + shm->relative_offset);
        }
    }

    RAISE("Failed to find absptr %p in shared memory", ptr)
}

shm_t* abs2shm(absptr_t ptr, ERRMSG) {
    PTR_RETURN_SETUP(shm_t)

    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = shopen(i, &CHILD_ERRMSG);
        FREE(CHILD_ERRMSG) // ignore error and continue searching for an shm
        if (shm == NULL) {
            continue;
        }
        char* shm_start = (char*)shm;
        char* data_start = shm_start + sizeof(shm_t);
        char* data_end = data_start + shm->volume_size;

        if ((char*)ptr >= data_start && (char*)ptr < data_end) {
            return shm;
        }
    }

    RAISE("Failed to find absptr %p in shared memory", ptr)
}

block_header_t* abs2blk(void* ptr, ERRMSG){
    PTR_RETURN_SETUP(block_header_t)

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));

    RAISE_IF(blk && blk->magic != BLK_MAGIC, "Bad block magic")

    return blk;
}

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG) {
    PTR_RETURN_SETUP(shm_t)

    RAISE_IF(shm_basename == NULL, "Undefined shm basename");

    // Calculate the total size needed for the shared memory segment
    size_t full_size = shm_size + sizeof(shm_t);

    // Prepare the shared memory name
    char shm_name[MAX_FILENAME_SIZE];
    snprintf(shm_name, sizeof(shm_name), "%s_%zu", shm_basename, volume_index);
    shm_name[MAX_FILENAME_SIZE - 1] = '\0'; // ensure the name is NULL terminated

    // Set the global basename, this will be used to name future volumes
    strncpy(common_basename, shm_basename, MAX_FILENAME_SIZE - 1);

    // Create or open a shared memory object
    // O_RDWR: Open for reading and writing
    // O_CREAT: Create if it doesn't exist
    // 0666: Set permissions (rw-rw-rw-)
    int fd = shm_open(shm_name, O_RDWR | O_CREAT, 0666);
    RAISE_IF(fd == -1, "Failed to open shared volume '%s'", shm_name)

    // Map the shared memory object into the process's address space
    volumes[volume_index] = (shm_t*)mmap(NULL, full_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    RAISE_IF_WITH(
        volumes[volume_index] == MAP_FAILED,
        close(fd),
        "Failed to memory map file '%s' to volume index %zu",
        shm_name,
        volume_index
    )

    // Get information about the shared memory object
    struct stat sb;
    RAISE_IF_WITH(
        fstat(fd, &sb) == -1,
        (munmap(volumes[volume_index], full_size), volumes[volume_index] = NULL, close(fd)),
        "Failed to fstat '%s'",
        shm_name
    )

    // Check if we've just created the shared memory object
    bool created = (sb.st_size == 0);
    RAISE_IF_WITH(
        created && ftruncate(fd, full_size) == -1,
        (munmap(volumes[volume_index], full_size), volumes[volume_index] = NULL, close(fd)),
        "Failed to set the size of the shared memory object '%s'",
        shm_name
    )

    // Adjust sizes based on whether we created a new object or opened an existing one
    full_size = created ? full_size : (size_t)sb.st_size;
    shm_size = full_size - sizeof(shm_t);

    // Cast the mapped memory to our shared memory structure
    shm_t* shm = (shm_t*)volumes[volume_index];
    if (created) {
        // Initialize the shared memory structure
        shm->magic = SHM_MAGIC;
        strncpy(shm->volume_name, shm_name, sizeof(shm->volume_name) - 1);
        shm->volume_name[sizeof(shm->volume_name) - 1] = '\0';
        shm->volume_index = volume_index;
        shm->relative_offset = 0;

        // Calculate the relative offset based on previous volumes
        // POTENTIAL ISSUE: This assumes volumes[] is initialized and accessible
        for (size_t i = 0; i < volume_index; i++) {
            shm->relative_offset += volumes[i]->volume_size;
        }

        // volume size does not count the shm header
        shm->volume_size = shm_size;

        // Initialize the read-write lock with process-shared attribute
        // so it works across processes (Python/R pools use multiprocessing)
        pthread_rwlockattr_t rwattr;
        pthread_rwlockattr_init(&rwattr);
        pthread_rwlockattr_setpshared(&rwattr, PTHREAD_PROCESS_SHARED);
        if (pthread_rwlock_init(&shm->rwlock, &rwattr) != 0){
            pthread_rwlockattr_destroy(&rwattr);
            munmap(volumes[volume_index], full_size);
            close(fd);
            RAISE("Failed initialize read-write lock on '%s'", shm_name)
        }
        pthread_rwlockattr_destroy(&rwattr);

        shm->cursor = 0;

        // Initialize the first block header
        block_header_t* first_block = (block_header_t*)(shm + 1);
        first_block->magic = BLK_MAGIC;
        first_block->reference_count = 0;
        // block size does not count the block headers
        first_block->size = shm_size - sizeof(block_header_t);
    }

    close(fd);
    return shm;
}

shm_t* shopen(size_t volume_index, ERRMSG) {
    PTR_RETURN_SETUP(shm_t)

    // If the volume has already been loaded, return it
    if(volumes[volume_index]){
        return volumes[volume_index];
    }

    // Prepare the shared memory name
    char shm_name[MAX_FILENAME_SIZE] = { '\0' };
    int written = snprintf(shm_name, MAX_FILENAME_SIZE - 1, "%s_%zu", common_basename, volume_index);
    RAISE_IF(
        written >= MAX_FILENAME_SIZE - 1 || written < 0,
        "Cannot make filename for shared memory volume %zu with common basename '%s', filename is too long",
        volume_index,
        common_basename
    )

    shm_name[MAX_FILENAME_SIZE - 1] = '\0'; // ensure the name is NULL terminated

    // Create or open a shared memory object
    // O_RDWR: Open for reading and writing
    // 0666: Set permissions (rw-rw-rw-)
    int fd = shm_open(shm_name, O_RDWR, 0666);
    if (fd == -1) {
        // This is OK, the volume doesn't exist
        return NULL;
    }

    struct stat sb;
    RAISE_IF_WITH(
        fstat(fd, &sb) == -1,
        close(fd),
        "Cannot fstat shared memory volume '%s'",
        shm_name
    )

    size_t volume_size = (size_t)sb.st_size;

    // Map the shared memory object into the process's address space
    volumes[volume_index] = (shm_t*)mmap(NULL, volume_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

    RAISE_IF_WITH(
        volumes[volume_index] == MAP_FAILED,
        close(fd),
        "Cannot memory map shared memory volume '%s'",
        shm_name
    )

    shm_t* shm = (shm_t*)volumes[volume_index];

    close(fd);
    return shm;
}

static bool shclose_unlocked(ERRMSG) {
    bool success = true;

    VAL_RETURN_SETUP(bool, false)

    for (int i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm;
        if (volumes[i] == NULL){
            shm = shopen(i, &CHILD_ERRMSG); IGNORE_ERROR
            if(shm == NULL){
                continue;
            }
        } else {
            shm = volumes[i];
        }

        // Get the name of the shared memory object
        char shm_name[MAX_FILENAME_SIZE];
        strncpy(shm_name, shm->volume_name, MAX_FILENAME_SIZE);

        // Destroy the rwlock before unmapping
        pthread_rwlock_destroy(&shm->rwlock);

        // Unmap the shared memory
        size_t full_size = shm->volume_size + sizeof(shm_t);
        if (munmap(shm, full_size) == -1) {
            success = false;
        }

        // Mark the shared memory object for deletion
        if (shm_unlink(shm_name) == -1) {
            success = false;
        }

        // Set the pointer to NULL to indicate it's no longer valid
        volumes[i] = NULL;
    }

    RAISE_IF(!success, "Failed to close all shared memory volumes")

    return success;
}

bool shclose(ERRMSG) {
    pthread_mutex_lock(&alloc_mutex);
    bool result = shclose_unlocked(errmsg_);
    pthread_mutex_unlock(&alloc_mutex);
    return result;
}

static bool get_available_memory(size_t* memory, ERRMSG) {
    BOOL_RETURN_SETUP

    FILE *meminfo = fopen("/proc/meminfo", "r");

    RAISE_IF(meminfo == NULL, "Failed to open '/proc/meminfo'")

    char line[256];
    bool success = false;
    size_t total_memory = 0;

    while (fgets(line, sizeof(line), meminfo)) {
        if (sscanf(line, "MemAvailable: %zu kB", &total_memory) == 1) {
            success = true;
            break;
        }
    }

    // return total memory in bytes, convert from kilobytes
    *memory = total_memory * 1024;

    fclose(meminfo);

    RAISE_IF(!success, "Failed to find 'MemAvailable' line in '/proc/meminfo'")

    return success;
}

static bool choose_next_volume_size(size_t* new_volume_size, size_t new_data_size, ERRMSG) {
    BOOL_RETURN_SETUP

    size_t shm_size = 0;
    size_t last_shm_size = 0;
    size_t minimum_required_size = sizeof(shm_t) + sizeof(block_header_t) + new_data_size;

    // Iterate through volumes to calculate total and last shared memory sizes
    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = volumes[i];
        if (!shm) break;
        shm_size += shm->volume_size;
        last_shm_size = shm->volume_size;
    }

    size_t available_memory = 0;
    TRY(get_available_memory, &available_memory);

    RAISE_IF( minimum_required_size > available_memory, "Insufficient memory for new data size")

    // Determine the new volume size based on available memory and existing volumes
    if (shm_size < available_memory && minimum_required_size < shm_size) {
        *new_volume_size = shm_size;
    } else if (last_shm_size < available_memory && minimum_required_size < last_shm_size) {
        *new_volume_size = last_shm_size;
    } else {
        *new_volume_size = minimum_required_size;
    }

    return true;
}

static block_header_t* get_block(shm_t* shm, ssize_t cursor, ERRMSG){
    PTR_RETURN_SETUP(block_header_t)

    RAISE_IF(shm == NULL, "Shared memory pool is not defined")

    // This will occur when a volume is filled, it does not necessarily mean
    // there is no space in the volume, but new space will need to be sought.
    RAISE_IF(cursor == VOLNULL, "Undefined cursor in this volume")

    block_header_t* blk = (block_header_t*) vol2abs(cursor, shm);

    RAISE_IF(blk->magic != BLK_MAGIC, "Memory corruption, bad block magic")

    return blk;
}

static block_header_t* scan_volume(block_header_t* blk, size_t size, char* end, ERRMSG){
    PTR_RETURN_SETUP(block_header_t)

    while ((char*)blk + sizeof(block_header_t) + size <= end) {
        if (!blk){
            // This is not an error, simply means the block wsa not found
            return NULL;
        }

        RAISE_IF(blk->magic != BLK_MAGIC, "Memory corruption, bad block magic")

        // Merge all following free blocks
        while (blk->reference_count == 0) {
            block_header_t* next_blk = (block_header_t*)((char*)blk + sizeof(block_header_t) + blk->size);

            if ((char*)next_blk >= end || next_blk->magic != BLK_MAGIC || next_blk->reference_count != 0) {
                break;
            }

            // Save size before modifying blk, then merge
            size_t next_size = next_blk->size;
            blk->size += sizeof(block_header_t) + next_size;
            memset(next_blk, 0, sizeof(block_header_t) + next_size);
        }

        // if this block is suitable, return it
        if (blk->reference_count == 0 && blk->size >= size) {
            return blk;
        }

        blk = (block_header_t*)((char*)blk + sizeof(block_header_t) + blk->size);
    }

    return NULL;
}

static block_header_t* find_free_block_in_volume(shm_t* shm, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(block_header_t)

    RAISE_IF(shm == NULL, "NULL pointer to shared memory volume")
    RAISE_IF(size == 0, "Cannot access empty shared memory volume")

    char* shm_end = (char*)shm + sizeof(shm_t) + shm->volume_size;

    // Lock this pool while searching. This is necessary since adjacent free
    // blocks will be merged, and the returned block must remain valid until
    // split_block processes it.
    if (pthread_rwlock_wrlock(&shm->rwlock) != 0) {
        RAISE("Failed to acquire write lock")
    }

    // try to get the current block at the cursor
    block_header_t* blk = get_block(shm, shm->cursor, &CHILD_ERRMSG);
    FREE(CHILD_ERRMSG)

    if (blk != NULL && blk->size >= size + sizeof(block_header_t) && blk->reference_count == 0) {
        pthread_rwlock_unlock(&shm->rwlock);
        return blk;
    }

    block_header_t* new_blk = scan_volume(blk, size, shm_end, &CHILD_ERRMSG);
    FREE(CHILD_ERRMSG)

    if(!new_blk){
        blk = get_block(shm, 0, &CHILD_ERRMSG);
        RAISE_IF_WITH(
            blk == NULL,
            pthread_rwlock_unlock(&shm->rwlock),
            "blk is NULL\n%s",
            CHILD_ERRMSG
        );

        shm_end = (char*)shm + sizeof(shm_t) + shm->cursor;
        new_blk = scan_volume(blk, size, shm_end, &CHILD_ERRMSG);
        RAISE_IF_WITH(
            new_blk == NULL,
            pthread_rwlock_unlock(&shm->rwlock),
            "new_blk is NULL\n%s",
            CHILD_ERRMSG
        );
    }

    pthread_rwlock_unlock(&shm->rwlock);

    return new_blk;
}

static block_header_t* find_free_block(size_t size, shm_t** shm_ptr, ERRMSG) {
    PTR_RETURN_SETUP(block_header_t)

    block_header_t* blk;
    shm_t* shm = volumes[current_volume];
    if (shm != NULL) {
        blk = get_block(shm, shm->cursor, &CHILD_ERRMSG);
        FREE(CHILD_ERRMSG) // failure is OK

        if(blk && blk->size >= size + sizeof(block_header_t)){
            RAISE_IF(
                blk && blk->reference_count != 0,
                "Expected cursor to point to new block with reference count of 0, found count of %u",
                blk->reference_count
            )
            goto success;
        }
    }

    // If no suitable block is found at the cursor, search through all volumes
    // for a suitable block, merging free blocks as they are observed
    for(size_t i = 0; i < MAX_VOLUME_NUMBER; i++){
      shm = volumes[i];

      // If no block is found in any volume, create a new volume
      if(!shm){
        size_t new_volume_size = 0;
        TRY(choose_next_volume_size, &new_volume_size, size);

        shm = TRY(shinit, common_basename, i, new_volume_size);

        blk = (block_header_t*)(shm + 1);
      }

      blk = find_free_block_in_volume(shm, size, &CHILD_ERRMSG);
      if(blk != NULL) {
          RAISE_IF(
              blk->reference_count != 0,
              "Expected cursor to point to new block with reference count of 0, found count of %u",
              blk->reference_count
          )
          current_volume = i;
          goto success;
      } else {
          // Do not need to set an error message here, we can continue searching
          // for a suitable block
          FREE(CHILD_ERRMSG);
      }
    }

    RAISE("Could not find suitable block");

success:
    *shm_ptr = shm;
    return blk;
}

static block_header_t* split_block(shm_t* shm, block_header_t* old_block, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(block_header_t)

    RAISE_IF(old_block->reference_count > 0, "Cannot split block since reference_count > 0")
    RAISE_IF(old_block->size < size, "This block is too small")

    if (old_block->size == size){
        // hello goldilocks, this block is just the right size
        return old_block;
    }

    // lock memory in this pool
    pthread_rwlock_wrlock(&shm->rwlock);

    size_t remaining_free_space = old_block->size - size;
    old_block->size = size;

    block_header_t* new_free_block = (block_header_t*)((char*)old_block + sizeof(block_header_t) + size);
    ssize_t new_cursor = abs2vol(new_free_block, shm, &CHILD_ERRMSG);
    RAISE_IF_WITH(new_cursor == VOLNULL, pthread_rwlock_unlock(&shm->rwlock), "\n%s", CHILD_ERRMSG)

    // if there is enough free space remaining to create a new block, do so
    if (remaining_free_space > sizeof(block_header_t)){
        // start of the new free block
        shm->cursor = new_cursor;
        new_free_block->magic = BLK_MAGIC;
        new_free_block->reference_count = 0;
        new_free_block->size = remaining_free_space - sizeof(block_header_t);
    } else {
        old_block->size += remaining_free_space;
        memset((void*)new_free_block, 0, remaining_free_space);
        shm->cursor = -1;
    }

    pthread_rwlock_unlock(&shm->rwlock);

    return old_block;
}

static void* shmalloc_unlocked(size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(size == 0, "Cannot (or will not) allocate 0-length block")

    shm_t* shm = NULL;
    // find a block with sufficient free space
    block_header_t* blk = find_free_block(size, &shm, &CHILD_ERRMSG);

    // If a suitable block is found
    if (blk != NULL) {
        // trim the block down to size and reset the cursor to the next free block
        block_header_t* final_blk = split_block(shm, blk, size, &CHILD_ERRMSG);
        if(final_blk){
            final_blk->reference_count = 1;
            return (void*)(final_blk + 1);
        }
    }

    RAISE("Failed to allocate shared memory block of size %zu:\n%s", size, CHILD_ERRMSG);
}

void* shmalloc(size_t size, ERRMSG) {
    pthread_mutex_lock(&alloc_mutex);
    void* result = shmalloc_unlocked(size, errmsg_);
    pthread_mutex_unlock(&alloc_mutex);
    return result;
}

void* shmemcpy(void* src, size_t size, ERRMSG){
    PTR_RETURN_SETUP(void)

    void* dest = TRY(shmalloc, size);

    memmove(dest, src, size);

    return dest;
}

void* shcalloc(size_t nmemb, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    void* data = TRY(shmalloc, nmemb * size);

    memset(data, 0, nmemb * size);

    return data;
}

static bool shfree_unlocked(absptr_t ptr, ERRMSG);

static void* shrealloc_unlocked(void* ptr, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(size == 0, "Cannot reallocate to size 0")

    // Match realloc(NULL, size) semantics
    if (ptr == NULL){
        return shmalloc_unlocked(size, errmsg_);
    }

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));
    shm_t* shm = abs2shm(ptr, &CHILD_ERRMSG);
    RAISE_IF(shm == NULL, "Failed to open shared memory drive\n%s", CHILD_ERRMSG)

    void* new_ptr;

    if (blk->size >= size) {
        // The current block is large enough -- shrink in place
        // Temporarily mark as free so split_block can operate on it
        blk->reference_count = 0;
        block_header_t* result = split_block(shm, blk, size, &CHILD_ERRMSG);
        if (result == NULL) {
            blk->reference_count = 1; // restore on failure
            RAISE("Failed to split block\n%s", CHILD_ERRMSG)
        }
        result->reference_count = 1;
        new_ptr = (void*)((char*)result + sizeof(block_header_t));
    } else {
        // Need to allocate a new block
        new_ptr = shmalloc_unlocked(size, &CHILD_ERRMSG);
        if (new_ptr == NULL) {
            RAISE("Failed to allocate new block\n%s", CHILD_ERRMSG)
        }

        pthread_rwlock_wrlock(&shm->rwlock);
        memcpy(new_ptr, ptr, blk->size);
        pthread_rwlock_unlock(&shm->rwlock);

        bool freed = shfree_unlocked(ptr, &CHILD_ERRMSG);
        RAISE_IF(!freed, "\n%s", CHILD_ERRMSG)
    }

    return new_ptr;
}

void* shrealloc(void* ptr, size_t size, ERRMSG) {
    pthread_mutex_lock(&alloc_mutex);
    void* result = shrealloc_unlocked(ptr, size, errmsg_);
    pthread_mutex_unlock(&alloc_mutex);
    return result;
}

static bool shfree_unlocked(absptr_t ptr, ERRMSG) {
    BOOL_RETURN_SETUP
    RAISE_IF(ptr == NULL, "Invalid or inaccessible shared memory pool pointer - perhaps the pool is closed?");

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));

    RAISE_IF(blk == NULL, "Out-of-bounds relative pointer");
    RAISE_IF(blk->magic != BLK_MAGIC, "Corrupted memory");
    RAISE_IF(blk->reference_count == 0, "Cannot free memory, reference count is already 0");

    // Atomic decrement: postfix -- on _Atomic returns previous value
    unsigned int prev = blk->reference_count--;

    if (prev == 1) {
        // We were the last reference -- safe to zero the data
        memset(blk + 1, 0, blk->size);
    }

    return true;
}

bool shfree(absptr_t ptr, ERRMSG) {
    pthread_mutex_lock(&alloc_mutex);
    bool result = shfree_unlocked(ptr, errmsg_);
    pthread_mutex_unlock(&alloc_mutex);
    return result;
}

size_t total_shm_size(){
    size_t total_size = 0;
    shm_t* shm;
    for(size_t i = 0; i < MAX_VOLUME_NUMBER; i++){
        shm = volumes[i];
        if(shm){
            total_size += shm->volume_size;
        }
    }
    return total_size;
}
