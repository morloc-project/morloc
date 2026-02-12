#ifndef __MORLOC_MEMORY_H__
#define __MORLOC_MEMORY_H__

#ifdef __cplusplus
#include <atomic>
#define _MORLOC_ATOMIC(T) std::atomic<T>
#else
#include <stdatomic.h>
#define _MORLOC_ATOMIC(T) _Atomic T
#endif
#include <stddef.h>
#include <pthread.h>
#include <stdlib.h> // for ssize_t

#include "macros.h"

// morloc shared library pool handling

//             head of volume 1   inter-memory    head of volume 2
//              n=6  block1          n=20                block2
//         ---xxxxxx........--------------------xxxxxx............---->
//         |  |     |      |                    |     |          |
//         v  v     v      v                    v     v          v
// absptr  0  4    10     17                   38    44         55
// volptr           0      7                          0         10
// relptr           0      7                          8         19
//
//  * the volumes may not be ordered in memory
//    e.g., block2 may be less then block1
//  * absptr will vary between processes, it is the virtual pointer used in
//    process memory and mmap'd to the shared data structure.
//  * relptr will be shared between processes
//  * relptr abstracts away volumes, viewing the shared memory pool as a
//    single contiguous block

#define SHM_MAGIC 0xFECA0DF0
#define BLK_MAGIC 0x0CB10DF0

#define MAX_VOLUME_NUMBER 32

// An index into a multi-volume shared memory pool
typedef ssize_t relptr_t;

// An index into a single volume. 0 is the start of the first block immediately
// following the shm object.
typedef ssize_t volptr_t;

// An absolute pointer to system memory
typedef void* absptr_t;

#define VOLNULL -1
#define RELNULL -1

typedef struct shm_s {
  // A constant identifying this as a morloc shared memory file
  unsigned int magic;

  // The name of this volume. Used for creating debugging messages.
  char volume_name[MAX_FILENAME_SIZE];

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

// Packed because block headers live at arbitrary offsets in mmap'd shared
// memory and the binary layout must be stable across compilation units.
typedef struct __attribute__((packed)) block_header_s {
    // a constant magic number identifying a block header
    unsigned int magic;
    // the number of references to this block (atomic for thread safety)
    _MORLOC_ATOMIC(unsigned int) reference_count;
    // the amount of memory that is stored in the header
    size_t size;
} block_header_t;

#endif
