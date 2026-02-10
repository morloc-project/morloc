#ifndef __MORLOC_H__
#define __MORLOC_H__

// {{{ setup

#include <assert.h>
#include <ctype.h>
#include <dirent.h> // used in delete_directory
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#define EXIT_CODE int
#define EXIT_PASS 0
#define EXIT_FAIL 1

#define MAX_FILENAME_SIZE 128
#define MAX_ERRMSG_SIZE 1024

// With these parameters, the max wait time is around 6s
#define WAIT_RETY_INITIAL_TIME 0.001
#define WAIT_RETRY_MULTIPLIER 1.25
#define WAIT_RETRY_ATTEMPTS 24

#define BUFFER_SIZE 4096

// }}} setup

// {{{ error handling macro definictions

#define FREE(ptr) \
    if(ptr != NULL){ \
        free(ptr); \
        ptr = NULL; \
    }

#define ERRMSG char** errmsg_
#define CHILD_ERRMSG child_errmsg_

#define ERROR_HANDLING_SETUP \
    char* CHILD_ERRMSG = NULL; \
    *errmsg_ = NULL; \
    char errmsg_buffer[MAX_ERRMSG_SIZE] = { 0 };

#define PTR_RETURN_SETUP(type) \
    ERROR_HANDLING_SETUP \
    type* fail_value_ = NULL;

#define INT_RETURN_SETUP \
    ERROR_HANDLING_SETUP \
    int fail_value_ = EXIT_FAIL;

#define VAL_RETURN_SETUP(type, value) \
    ERROR_HANDLING_SETUP \
    type fail_value_ = value;

#define BOOL_RETURN_SETUP \
    ERROR_HANDLING_SETUP \
    int fail_value_ = false;

#define IGNORE_ERROR \
    FREE(CHILD_ERRMSG);

#define RAISE(msg, ...) \
    snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    *errmsg_ = strdup(errmsg_buffer); \
    if(child_errmsg_ != NULL){ \
        free(child_errmsg_); \
    } \
    return fail_value_;

#define RAISE_WITH(end, msg, ...) \
    snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    *errmsg_ = strdup(errmsg_buffer); \
    end; \
    if(child_errmsg_ != NULL){ \
        free(child_errmsg_); \
    } \
    return fail_value_;

#define RAISE_IF(cond, msg, ...) \
    if((cond)){ \
        RAISE(msg, ##__VA_ARGS__) \
    }

#define RAISE_IF_WITH(cond, end, msg, ...) \
    if((cond)){ \
        RAISE_WITH(end, msg, ##__VA_ARGS__) \
    }

#define TRY_GOTO(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    if(CHILD_ERRMSG != NULL){ \
        snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, CHILD_ERRMSG); \
        *errmsg_ = strdup(errmsg_buffer); \
        FREE(CHILD_ERRMSG) \
        goto end; \
    }

#define TRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG)

#define TRY_WITH(end, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    RAISE_IF_WITH(CHILD_ERRMSG != NULL, end, "\n%s", CHILD_ERRMSG)

#define WAIT(code, cond, timeout_code) do { \
    double retry_time = WAIT_RETY_INITIAL_TIME; \
    int attempts = 0; \
    bool timed_out = false; \
    while(!timed_out){ \
        code; \
        if((cond)){ \
            break; \
        } \
        struct timespec sleep_time = { \
            .tv_sec = (time_t)retry_time, \
            .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9) \
        }; \
        nanosleep(&sleep_time, NULL); \
        attempts++; \
        retry_time *= WAIT_RETRY_MULTIPLIER; \
        if(attempts > WAIT_RETRY_ATTEMPTS) { \
            timeout_code; \
        } \
    } \
    } while(0);

// }}}

// {{{ utilities

// just a debugging function

// return true if file exists (may or may not be openable)


// make a directory recursively (like `mkdir -p`)



// Recursively delete a directory and its contents

// Check if a string has a given suffix


// Get the base directory from a directory path
//
// The input path is mutated
//
// Examples:
//   "foo/bar/baz/" --> "foo/bar"
//   "foo/bar/baz" --> "foo/bar"
//   "foo/bar" --> "foo"
//   "bar" --> "."
//   "/" --> "/"




// Write a given number of bytes to STDOUT






// }}}

// {{{ Data structures

// This type should act as a string-based dictionary
// The current implementation as a linked list is slow and can be replaced later
// with a tree structure, but it suffices for the current small use case.
typedef struct dict_s dict_t;

typedef struct dict_s {
    char* name;
    void* thing;
    dict_t* next;
} dict_t;

// add an element to the beginning of a named linked list
// inserting an element into an empty dict creates a dict singleton

// O(n) for this linked list implementation

// As currently implemented, the dictionary may have synonyms. This function
// deletes (and frees) all entries with the same name.

// Free the list spine AND the names, but not the elements

// }}}

// {{{ MessagePack parser from libmpack

// The following code is adapted from libmpack: https://github.com/libmpack/libmpack

#ifndef MPACK_API
# define MPACK_API extern
#endif

#ifdef __GNUC__
# define FPURE __attribute__((const))
# define FNONULL __attribute__((nonnull))
# define FNONULL_ARG(x) __attribute__((nonnull x))
# define FUNUSED __attribute__((unused))
#else
# define FPURE
# define FNONULL
# define FNONULL_ARG(x)
# define FUNUSED
#endif

typedef struct mpack_value_s {
  uint32_t lo;
  uint32_t hi;
} mpack_value_t;


enum {
  MPACK_OK = 0,
  MPACK_EOF = 1,
  MPACK_ERROR = 2
} mpack_error;

#define MPACK_MAX_TOKEN_LEN 9  /* 64-bit ints/floats plus type code */

typedef enum {
  MPACK_TOKEN_NIL       = 1,
  MPACK_TOKEN_BOOLEAN   = 2,
  MPACK_TOKEN_UINT      = 3,
  MPACK_TOKEN_SINT      = 4,
  MPACK_TOKEN_FLOAT     = 5,
  MPACK_TOKEN_CHUNK     = 6,
  MPACK_TOKEN_ARRAY     = 7,
  MPACK_TOKEN_MAP       = 8,
  MPACK_TOKEN_BIN       = 9,
  MPACK_TOKEN_STR       = 10,
  MPACK_TOKEN_EXT       = 11
} mpack_token_type_t;

typedef struct mpack_token_s {
  mpack_token_type_t type;  /* Type of token */
  uint32_t length;    /* Byte length for str/bin/ext/chunk/float/int/uint.
                               Item count for array/map. */
  union {
    mpack_value_t value;    /* 32-bit parts of primitives (bool,int,float) */
    const char *chunk_ptr;  /* Chunk of data from str/bin/ext */
    int ext_type;           /* Type field for ext tokens */
  } data;
} mpack_token_t;

typedef struct mpack_tokbuf_s {
  char pending[MPACK_MAX_TOKEN_LEN];
  mpack_token_t pending_tok;
  size_t ppos, plen;
  uint32_t passthrough;
} mpack_tokbuf_t;

#define MPACK_TOKBUF_INITIAL_VALUE { { 0 }, { (mpack_token_type_t)0, 0, { .value = { 0 } } }, 0, 0, 0 }

#ifdef __cplusplus
extern "C" {
#endif

MPACK_API int mpack_read(mpack_tokbuf_t *tb, const char **b, size_t *bl,
    mpack_token_t *tok) FUNUSED FNONULL;
MPACK_API int mpack_write(mpack_tokbuf_t *tb, char **b, size_t *bl,
    const mpack_token_t *tok) FUNUSED FNONULL;

// bool is already available: C has stdbool.h (included above), C++ has native bool

MPACK_API mpack_token_t mpack_pack_nil(void) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_boolean(unsigned v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_uint(uint64_t v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_sint(int64_t v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_float(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_str(uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_array(uint32_t l) FUNUSED FPURE;
MPACK_API bool mpack_unpack_boolean(mpack_token_t t) FUNUSED FPURE;
MPACK_API uint64_t mpack_unpack_uint(mpack_token_t t) FUNUSED FPURE;
MPACK_API int64_t mpack_unpack_sint(mpack_token_t t) FUNUSED FPURE;
MPACK_API double mpack_unpack_float(mpack_token_t t) FUNUSED FPURE;

#ifdef __cplusplus
}
#endif

#define UNUSED(p) (void)p;
#define ADVANCE(buf, buflen) ((*buflen)--, (unsigned char)*((*buf)++))
#define TLEN(val, range_start) ((uint32_t)(1 << (val - range_start)))
#ifndef MIN
# define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif


























#define POW2(n) \
  ((double)(1 << (n / 2)) * (double)(1 << (n / 2)) * (double)(1 << (n % 2)))

#define MPACK_SWAP_VALUE(val)                                  \
  do {                                                         \
    uint32_t lo = val.lo;                                      \
    val.lo = val.hi;                                           \
    val.hi = lo;                                               \
  } while (0)











/* unpack signed integer without relying on two's complement as internal
 * representation */




// }}}

// {{{ morloc shared library pool handling

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
    // the number of references to this block
    unsigned int reference_count;
    // the amount of memory that is stored in the header
    size_t size;
} block_header_t;







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





// No errors are raised here, pointers that map outside the volume return -1
//
// This function is a bit odd ... maybe we should kill it?





// Get and check a block header given a pointer to the beginning of a block's
// data section




// like shinit, but only opens existing share memory volumes if the exist
// if no volume exists, return NULL
// Non-existence of the volume is not considered an error










// Find a free block that can allocate a given size of memory. If no lbock is
// found, create a new volume.









// Free a chunk of memory. The pointer points to the start of the memory that
// the user is given relative to the user's process. The block header is just
// upstream of this position.
//
// return true for success


// }}}

// {{{ morloc schema support

typedef enum {
  MORLOC_NIL,
  MORLOC_BOOL,
  MORLOC_SINT8,
  MORLOC_SINT16,
  MORLOC_SINT32,
  MORLOC_SINT64,
  MORLOC_UINT8,
  MORLOC_UINT16,
  MORLOC_UINT32,
  MORLOC_UINT64,
  MORLOC_FLOAT32,
  MORLOC_FLOAT64,
  MORLOC_STRING,
  MORLOC_ARRAY,
  MORLOC_TUPLE,
  MORLOC_MAP
} morloc_serial_type;

#define SCHEMA_NIL    'z'
#define SCHEMA_BOOL   'b'
#define SCHEMA_SINT   'i'
#define SCHEMA_UINT   'u'
#define SCHEMA_FLOAT  'f'
#define SCHEMA_STRING 's'
#define SCHEMA_ARRAY  'a'
#define SCHEMA_TUPLE  't'
#define SCHEMA_MAP    'm'

// Schema definition
//  * Primitives have no parameters
//  * Arrays have one
//  * Tuples and records have one or more
struct Schema;
typedef struct Schema {
    morloc_serial_type type;
    size_t size; // number of parameters
    size_t width; // bytes in the object when stored in an array
    size_t* offsets;
    char* hint;
    struct Schema** parameters;
    char** keys; // field names, used only for records
} Schema;


// Convert an integer to a character, assuming that the size is 0-63. This is
// the currently allowed limit on tuple length. I'll extend this soon.




// Allocate a shared memory block sufficient to store a schema's object

// The voidstar representation of variable length data
typedef struct Array {
  size_t size;
  relptr_t data;
} Array;


// Check is the datastructure defined by a schema has fixed length.
//
// This will be true if there are no arrays in the structure.




// Prototypes


// Helper function to create a schema with parameters













// This parser starts on the character **after** the initial '<'





// }}} end morloc schema

// {{{ morloc MessagePack support

// Main pack function for creating morloc-encoded MessagePack data


// Try to add `added_size` bytes of space to a buffer, if there is not enough
// space, increase the buffer size.


// write data to a packet, if the buffer is too small, increase its size


// write a token to a packet, increase buffer size as needed


//  The main function for writing MessagePack


#define MPACK_TOKBUF_INITIAL_VALUE { { 0 }, { (mpack_token_type_t)0, 0, { .value = { 0 } } }, 0, 0, 0 }



// Take a morloc datastructure and convert it to MessagePack



// nested msg_sizers








// terminal parsers

// nested parsers











// take MessagePack data and set a pointer to an in-memory data structure

// }}} end Morloc pack support

// {{{ morloc JSON support

// Allocate a new string that with double quotes around the input string

// Function to escape a JSON string








// Calculate the number of characters in a JSON string
//
// This count is equal to the number of characters in the C string. Escaped
// special characters count as one. Any formatting errors will be caught.



// loop to the end of an array, counting the elements

// input JSON data should be NULL terminated

// Returns a pointer to voidstar data




#define JSON_PATH_TYPE_KEY 0
#define JSON_PATH_TYPE_IDX 1

typedef union json_path_u {
    char* key;
    size_t index;
} json_path_u;

typedef struct path_s {
    uint8_t json_path_type;
    json_path_u element;
} path_t;


// json_ptr is a pointer to a json element.
// This function iterates through the json data until the end of the element
// and returns a pointer to the character after the element.


// }}} end JSON

// {{{ morloc packet binary protocol

// The first 4 bytes of every morloc packet begin with these characters
#define MORLOC_PACKET_MAGIC 0x0707f86d // backwards since we are little endian

#define THIS_PLAIN     0
#define THIS_VERSION   0
#define DEFAULT_FLAVOR 0
#define DEFAULT_MODE   0

// Morloc packet types
typedef uint8_t command_type_t;
#define PACKET_TYPE_DATA  ((command_type_t)0)
#define PACKET_TYPE_CALL  ((command_type_t)1)
#define PACKET_TYPE_PING  ((command_type_t)2)

// type: an element of the command union that is just for type resolution
typedef struct __attribute__((packed)) packet_command_type_s {
    command_type_t type; // identifier for resolving unions
    uint8_t padding[7]; // pad to 8 bytes
} packet_command_type_t;

#define PACKET_ENTRYPOINT_LOCAL      0x00
#define PACKET_ENTRYPOINT_REMOTE_SFS 0x01 // packet is from a parent, and reader
                                          // is a worker on a shared file system
                                          // (e.g., in a slurm environment)

// call: send zero or more data packets as arguments to manifold midx
typedef struct __attribute__((packed)) packet_command_call_s {
    command_type_t type; // identifier for resolving unions
    uint8_t entrypoint; // is this call local, remote, or something more nuanced
    uint8_t padding[2]; // padding in the middle to align midx
    uint32_t midx;
} packet_command_call_t;

// data: data or a fail message
typedef struct __attribute__((packed)) packet_command_data_s {
    command_type_t type; // identifier for resolving unions
    uint8_t source; // where is the data stored (MESG, FILE, RPTR)
    uint8_t format; // what is the data format (JSON, MSGPACK, etc)
    uint8_t compression; // compression algorithm (if any)
    uint8_t encryption; // encryption algorithm (if any)
    uint8_t status; // PASS or FAIL status, no data if FAIL, only error message
    uint8_t padding[2];
} packet_command_data_t;

// The method used to strore the data
#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_RPTR  0x02 // the message is a relative pointer to shared memory

// The format in which the data is stored
#define PACKET_FORMAT_JSON     0x00
#define PACKET_FORMAT_MSGPACK  0x01
#define PACKET_FORMAT_TEXT     0x02
#define PACKET_FORMAT_DATA     0x03 // raw binary data
#define PACKET_FORMAT_VOIDSTAR 0x04 // binary morloc formatted data

// Compression algorithm
#define PACKET_COMPRESSION_NONE  0x00 // uncompressed

// Encryption algorithm
#define PACKET_ENCRYPTION_NONE   0x00 // unencrypted

// Packet status
#define PACKET_STATUS_PASS  0x00
#define PACKET_STATUS_FAIL  0x01

// ping: an empty salutation
typedef struct __attribute__((packed)) packet_command_ping_s {
    command_type_t type; // identifier for resolving unions
    uint8_t padding[7];
} packet_command_ping_t;

// The union of the 8-byte commands above. Packed because it sits at an
// unaligned offset (12) inside the packed morloc_packet_header_t.
typedef union __attribute__((packed)) packet_command_u {
    packet_command_type_t cmd_type;
    packet_command_call_t call;
    packet_command_data_t data;
    packet_command_ping_t ping;
} packet_command_t;

// This structure directly represents the binary format of morloc packets. It is
// essential that the compiler does not add padding (hence the "packed"
// attribute).
typedef struct __attribute__((packed)) morloc_packet_header_s {
  // morloc-specific prefix, should always be: 6D F8 07 07
  uint32_t magic;
  // the morloc plain, plains do not interact, they represent different sets of
  // incompatible functions and ways of looking at the world
  uint16_t plain;
  // morloc packet version
  uint16_t version;
  // flavor of the morloc packet - not yet used
  uint16_t flavor;
  // not yet used
  uint16_t mode;
  // union of all supported 7-byte commands, selected using `command_type`
  packet_command_t command;
  // offset - 32 bit integer specifying offset to start of payload, the space
  // between the end of the header and the start of the payload may contain
  // arbitrary extra information.
  uint32_t offset;
  // length of the main payload data
  uint64_t length;
} morloc_packet_header_t;

static_assert(
    sizeof(morloc_packet_header_t) == 32,
    "Header size mismatch!"
);


#define MORLOC_METADATA_TYPE_SCHEMA_STRING 0x01
#define MORLOC_METADATA_TYPE_XXHASH 0x02

#define MORLOC_METADATA_HEADER_MAGIC "mmh"

// Packed for stable binary layout in packet metadata sections.
typedef struct __attribute__((packed)) morloc_metadata_header_s {
    char magic[3];
    uint8_t type;
    uint32_t size;
} morloc_metadata_header_t;

// }}}

// {{{ morloc packet API

typedef struct morloc_call_s {
    uint32_t midx;
    uint8_t** args;
    size_t nargs;
} morloc_call_t;













// TODO: what should our submarine reflect?
// Currently I just return an exact copy of the ping packet


// Set the packet header at the first 32 bytes of a data block










// Make a data packet from a relative pointer to shared memory data
// Include the schema in the packet metadata




// Returns a metadata header if the magic matches, otherwise returns NULL


// Read a data packet type schema from the packet metadata
// If no type schema is defined, return NULL
// Raise an error if the packet is malformed
// On success, return a pointer to the schema string in the packet (no copying)




// Returns the error message if this packet failed and NULL otherwise


// Opens a data packet returning a pointer to a packet
//  * If packet has failing status, it is returned unchanged (error propagation)
//  * If packet handling fails, the fail_packet is defined and NULL is returned
//  * Else an absolute pointer to voidstar data is returned




// Make a packet wrapping a call on the local machine


// Make a packet wrapping a call on a remote worker




// }}} end packet support

// {{{ morloc packet IO

// * recurse through the data structure
// * for each array, calculate the full size of the data it points to (subtract
//   the 16 byte Array struct), add this to data_index

// forward declaration, the *data_r and *arra_r functions are mutually recursive

// * recurse through the data structure
// * call print_voidstar_binary_array_r on each array

// * map print_voidstar_binary_binder_r over elements
// * map print_voidstar_binary_data_r over elements

// Print voidstar data relative to 0
//
// All array relative pointers need to be readjusted, since we are printing, all
// actions have to be linear. The existing structure is not changed and no new
// memory is allocated.
//
// On success, returns a relptr to the position after the data (probably not
// useful unless you want to calculate the size of the printed data)


// Print morloc data packet to a file descriptor
//  * If the packet has the failed bit set, an error will be raised
//  * If the data is stored as a file or message, the exact packet is printed
//  * If the packet contains a relative pointer, it will be linearized and included as raw data

// }}} end morloc packet IO

// {{{ socket API

// needed for interop
#include <errno.h>       // For errno
#include <fcntl.h>
#include <poll.h>
#include <signal.h>      // For sigprocmask, sigset_t
#include <stdlib.h>      // For calloc, free
#include <string.h>      // For memcpy, strerror
#include <sys/select.h>  // For pselect, fd_set
#include <sys/socket.h>  // For recv
#include <sys/un.h>
#include <sys/wait.h>


typedef struct client_list_s {
    int fd;
    struct client_list_s* next;
} client_list_t;

typedef struct language_daemon_s {
    char* socket_path;
    char* tmpdir;
    char* shm_basename;
    shm_t* shm;
    size_t shm_default_size;
    int server_fd;
    fd_set read_fds;
    client_list_t* client_fds;
} language_daemon_t;






// Create a Unix domain socket










// Stream with eternal wait








// wait forever

// }}} end socket API

// {{{ hash support

// Define XXH_INLINE_ALL to include the implementation in this file
#define XXH_INLINE_ALL
#include "xxhash.h"
// Main export from xxhash.h is XXH64 function with the following prototype:
//   uint64_t XXH64(const char* data1, size_t dataLength, uint64_t seed)

// Two prime numbers used in xxhash
const uint64_t PRIME64_1 = 0x9E3779B185EBCA87;
const uint64_t PRIME64_2 = 0xC2B2AE3D27D4EB4F;


// Mix two hashes




// Hash a morloc packet








// Sends data to cache given an integer key. The main use case is caching the
// return values from remote calls. In thise case, the key will be the hash of
// the call which accounts for all inputs and the code the operates on
// it. Importantly, the key is NOT the hash of this return value (because we
// don't know the result before we run the computation).
//
// If the packet is successfully cached, return the cache filename
// Else return NULL

// TODO:
//   * send in a full data packet, not just the contents
//   * write the contents to a MessagePack file in the cache folder
//   * create a new data pack the wraps the MessagePack data file and write it
//     also to the cache folder
//   * return the path to the new data packet


// Get a cached packet given the key (usually a hash)
//
// The cached packet should be stateless, independent of the shared memory pool
// (which is local and transient). The packet should either store its contents
// as raw data or should contain a path to a file with the raw data.


// Deletes a cached packet given a key
//
// Also deletes the associated data file, if any is defined

// Checks if a cached packet exists given a key
//
// If a cached packet exists, return the filename
// Else return NULL

// }}} end hash support

// {{{ parsing CLI arguments

typedef struct argument_s {
    char* value;
    char** fields;
    char** default_fields;
    size_t size;
} argument_t;



// assumes argument_t owns all arguments
// this will be true if the initializers above are used


// Check if the data may be MessagePack (and is not JSON)
//
// We look ONLY at the first character and the size



// Parse a command line argument string that should contain data of a given type

// recursively free any memory stored at the specified location
// zero the location

// Parse a command line argument unrolled record

// Parse one CLI argument and return a data packet


// Given the manifold ID and argument and schema strings, create a morloc call packet

/// }}}

// {{{ pure morloc types and constructors

// all morloc expression types
typedef enum {
  MORLOC_X_DAT,
  MORLOC_X_APP,
  MORLOC_X_LAM,
  MORLOC_X_BND,
  MORLOC_X_PAT,
  MORLOC_X_FMT,
} morloc_expression_type;

// application types
typedef enum { APPLY_PATTERN, APPLY_LAMBDA, APPLY_FORMAT } morloc_app_expression_type;

// pattern types
typedef enum { SELECT_BY_KEY, SELECT_BY_INDEX, SELECT_END } morloc_pattern_type;

// Forward declarations
typedef struct morloc_expression_s morloc_expression_t;
typedef struct morloc_app_expression_s morloc_app_expression_t;
typedef struct morloc_lam_expression_s morloc_lam_expression_t;
typedef struct morloc_data_s morloc_data_t;
typedef struct morloc_pattern_s morloc_pattern_t;

// represent a pure morloc expression, a node in the syntax tree
typedef struct morloc_expression_s {
    morloc_expression_type type;
    Schema* schema;
    union {
        morloc_app_expression_t* app_expr;
        morloc_lam_expression_t* lam_expr;
        char* bnd_expr;
        char** interpolation; // NULL pointer terminated array of strings
        morloc_pattern_t* pattern_expr;
        morloc_data_t* data_expr;
    } expr;
} morloc_expression_t;

// represent all primitives
// field names are same as schema terms
typedef union primitive_u {
    char*    s; // pointer to null-terminated string
    uint8_t  z;    // storing value 0
    bool     b;
    int8_t   i1;
    int16_t  i2;
    int32_t  i4;
    int64_t  i8;
    uint8_t  u1;
    uint16_t u2;
    uint32_t u4;
    uint64_t u8;
    float    f4;
    double   f8;
} primitive_t;

// morloc literal array
typedef struct morloc_data_array_s {
    Schema* schema; // element schema
    size_t size;
    morloc_expression_t** values;
} morloc_data_array_t;

// store morloc data, primitives or containers
typedef struct morloc_data_s {
    bool is_voidstar;
    union {
        // literal data values stored here
        primitive_t lit_val;
        morloc_expression_t** tuple_val;
        morloc_data_array_t* array_val;
        // data stored in shared memory, either from arguments or aggregated
        // results; voidstar data is the final product of evaluation
        void* voidstar;
    } data;
} morloc_data_t;

// store an application of many args to either a pattern or a lambda
typedef struct morloc_app_expression_s {
    morloc_app_expression_type type;
    union {
        morloc_pattern_t* pattern;
        morloc_lam_expression_t* lambda;
        char** fmt;
    } function;
    morloc_expression_t** args;
    size_t nargs;
} morloc_app_expression_t;

// store a lambda
typedef struct morloc_lam_expression_s {
    size_t nargs;
    char** args;
    morloc_expression_t* body;
} morloc_lam_expression_t;

// store a pattern selector
typedef struct morloc_pattern_s {
    morloc_pattern_type type;
    size_t size;
    union {
        size_t* indices;
        char** keys;
    } fields;
   morloc_pattern_t** selectors;
} morloc_pattern_t;









// The variables are alternating indices (size_t) and patterns
// (morloc_pattern_t*) types. The patterns may be null, indicating they are
// terminals.

// Like the *idx function above, the two functions can't be collapsed yet
// because the record selector is polymorphic over records with the same
// fields. Technically, at compile-time, the record involved is currently known,
// but I may add more dynamicism in the future, so I will leave the pattern
// polymorphic and resolve to record indices using the Schema.

// }}}

// {{{ pure morloc interpretor



// evaluate a pure morloc expression with user provided arguments








// }}}

// prolly bring this back
// #ifdef SLURM_SUPPORT

// {{{ slurm support

#define MAX_SLURM_COMMAND_LENGTH 1024

// For each field, -1 indicates undefined
typedef struct resources_s {
  int memory; // in Gb
  int time; // walltime in seconds
  int cpus;
  int gpus;
} resources_t;

#define DEFAULT_XXHASH_SEED 0


// Parse a slurm walltime string
//
// For now, in morloc, I will be fairly strict and allow only two forms:
//  1. days-hours:minutes:seconds
//  1. hours:minutes:seconds
//
// Examples:
//   * 5-12:00:00 - 5 and a half days
//   * 12:00:00 - 12 hours
//   * 00:30:00 - 30 minutes

// Convert a number of seconds to a walltime string of format:
//   days-hours:minutes:seconds
// hours, minutes, and seconds should be 0-padded to 2 digits

// Parse the arguments of a morloc call packet.
//
// This function mutates the `args ` and `nargs` arguments to store the call
// arguments and counts (respectively). `args` contains pointers to locations
// in the original `packet` array, so `packet` must not be freed until after
// `args` is freed.


// Check if a given SLURM job has completed




// Before calling this function, every argument must be converted to a packet
//   * if the data is native, then it should be converted

// }}} end slurm support

// #endif // ending SLURM_SUPPORT

// {{{ function prototypes

#ifdef __cplusplus
extern "C" {
#endif

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG);
shm_t* shopen(size_t volume_index, ERRMSG);
bool shclose(ERRMSG);
void* shmalloc(size_t size, ERRMSG);
void* shmemcpy(void* src, size_t size, ERRMSG);
bool shfree(absptr_t ptr, ERRMSG);
void* shcalloc(size_t nmemb, size_t size, ERRMSG);
void* shrealloc(void* ptr, size_t size, ERRMSG);
size_t total_shm_size();
volptr_t rel2vol(relptr_t ptr, ERRMSG);
absptr_t rel2abs(relptr_t ptr, ERRMSG);
relptr_t vol2rel(volptr_t ptr, shm_t* shm);
absptr_t vol2abs(volptr_t ptr, shm_t* shm);
volptr_t abs2vol(absptr_t ptr, shm_t* shm, ERRMSG);
relptr_t abs2rel(absptr_t ptr, ERRMSG);
shm_t* abs2shm(absptr_t ptr, ERRMSG);
block_header_t* abs2blk(void* ptr, ERRMSG);
Schema* parse_schema(const char* schema, ERRMSG);
Schema* parse_schema_r(char** schema_ptr, ERRMSG);
int pack(const void* mlc, const char* schema_str, char** mpkptr, size_t* mpk_size, ERRMSG);
int pack_with_schema(const void* mlc, const Schema* schema, char** mpkptr, size_t* mpk_size, ERRMSG);
int unpack(const char* mpk, size_t mpk_size, const char* schema_str, void** mlcptr, ERRMSG);
int unpack_with_schema(const char* mpk, size_t mpk_size, const Schema* schema, void** mlcptr, ERRMSG);
void hex(const void *ptr, size_t size);
bool file_exists(const char *filename);
int mkdir_p(const char *path, ERRMSG);
void delete_directory(const char* path);
bool has_suffix(const char* x, const char* suffix);
char* dirname(char* path);
int write_atomic(const char* filename, const uint8_t* data, size_t size, ERRMSG);
int print_binary(const char *buf, size_t count, ERRMSG);
uint8_t* read_binary_fd(FILE* file, size_t* file_size, ERRMSG);
uint8_t* read_binary_file(const char* filename, size_t* file_size, ERRMSG);
dict_t* dict_insert(char* name, void* thing, dict_t* dict);
void* dict_lookup(char* name, dict_t* ll);
dict_t* dict_delete(char* name, dict_t* dict);
void dict_free(dict_t* list);
char* schema_to_string(const Schema* schema);
void* get_ptr(const Schema* schema, ERRMSG);
void free_schema(Schema* schema);
char* quoted(const char* input);
bool print_voidstar(const void* voidstar, const Schema* schema, ERRMSG);
char* access_json_by_path(char* json, path_t* path, size_t path_length, ERRMSG);
morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG);
bool packet_is_ping(const uint8_t* packet, ERRMSG);
bool packet_is_local_call(const uint8_t* packet, ERRMSG);
bool packet_is_remote_call(const uint8_t* packet, ERRMSG);
size_t morloc_packet_size_from_header(const morloc_packet_header_t* header);
size_t morloc_packet_size(const uint8_t* packet, ERRMSG);
uint8_t* return_ping(const uint8_t* packet, ERRMSG);
uint8_t* make_ping_packet();
uint8_t* make_standard_data_packet(relptr_t ptr, const Schema* schema);
uint8_t* make_mpk_data_packet(const char* mpk_filename, const Schema* schema);
morloc_metadata_header_t* as_morloc_metadata_header(const uint8_t* ptr);
char* read_schema_from_packet_meta(const uint8_t* packet, ERRMSG);
uint8_t* make_fail_packet(const char* failure_message);
char* get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG);
uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG);
uint8_t* make_morloc_local_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
uint8_t* make_morloc_remote_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG);
morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG);
int print_morloc_data_packet(const uint8_t* packet, const Schema* schema, ERRMSG);
void close_socket(int socket_id);
void close_daemon(language_daemon_t** daemon_ptr);
language_daemon_t* start_daemon( const char* socket_path, const char* tmpdir, const char* shm_basename, size_t shm_default_size, ERRMSG );
uint8_t* stream_from_client_wait(int client_fd, int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* stream_from_client(int client_fd, ERRMSG);
uint8_t* send_and_receive_over_socket_wait(const char* socket_path, const uint8_t* packet, int pselect_timeout_us, int recv_timeout_us, ERRMSG);
uint8_t* send_and_receive_over_socket(const char* socket_path, const uint8_t* packet, ERRMSG);
size_t send_packet_to_foreign_server(int client_fd, uint8_t* packet, ERRMSG);
int wait_for_client_with_timeout(language_daemon_t* daemon, int timeout_us, ERRMSG);
int wait_for_client(language_daemon_t* daemon, ERRMSG);
bool hash_morloc_packet(const uint8_t* packet, const Schema* schema, uint64_t seed, uint64_t* hash, ERRMSG);
char* put_cache_packet(const uint8_t* voidstar, const Schema* schema, uint64_t key, const char* cache_path, ERRMSG);
uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
char* check_cache_packet(uint64_t key, const char* cache_path, ERRMSG);
argument_t* initialize_positional(char* value);
argument_t* initialize_unrolled(size_t size, char* default_value, char** fields, char** default_fields);
void free_argument_t(argument_t* arg);
uint8_t* parse_cli_data_argument(uint8_t* dest, const argument_t* arg, const Schema* schema, ERRMSG);
uint8_t* make_call_packet_from_cli( uint8_t* dest, uint32_t mid, argument_t** args, char** arg_schema_strs, ERRMSG );
morloc_expression_t* make_morloc_bound_var(const char* schema_str, char* varname);
morloc_expression_t* make_morloc_literal( const char* schema_str, primitive_t lit );
morloc_expression_t* make_morloc_container( const char* schema_str, size_t nargs, ... );
morloc_expression_t* make_morloc_app( const char* schema_str, morloc_expression_t* func, size_t nargs, ... );
morloc_expression_t* make_morloc_lambda( morloc_expression_t* body, size_t nvars, ... );
morloc_expression_t* make_morloc_interpolation(const char* schema_str, size_t nargs, ...);
morloc_expression_t* make_morloc_pattern(const char* schema_str, morloc_pattern_t* pattern);
morloc_pattern_t* make_morloc_pattern_end();
morloc_pattern_t* make_morloc_pattern_idx(size_t nargs, ...);
morloc_pattern_t* make_morloc_pattern_key(size_t nargs, ...);
absptr_t morloc_eval( morloc_expression_t* expr, Schema* return_schema, uint8_t** arg_voidstar, Schema** arg_schemas, size_t nargs, ERRMSG );
size_t parse_slurm_time(const char* time_str, ERRMSG);
char* write_slurm_time(int seconds);
bool parse_morloc_call_arguments( uint8_t* packet, uint8_t** args, size_t* nargs, ERRMSG );
bool slurm_job_is_complete(uint32_t job_id);
uint32_t submit_morloc_slurm_job( const char* nexus_path, const char* socket_basename, const char* call_packet_filename, const char* result_cache_filename, const char* output_filename, const char* error_filename, const resources_t* resources, ERRMSG);
uint8_t* remote_call( int midx, const char* socket_basename, const char* cache_path, const resources_t* resources, const uint8_t** arg_packets, size_t nargs, ERRMSG );

// xxhash wrapper - avoids exposing xxhash.h in public header
uint64_t morloc_xxh64(const void* input, size_t length, uint64_t seed);

#ifdef __cplusplus
}
#endif

// }}} function prototypes

#endif // __MORLOC_H__
