#ifndef __MORLOC_PACKET_H__
#define __MORLOC_PACKET_H__

#include <stdint.h>
#include <assert.h>
#include "macros.h"

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

#endif // __MORLOC_PACKET_H__
