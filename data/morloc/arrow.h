#ifndef __MORLOC_ARROW_H__
#define __MORLOC_ARROW_H__

#include "memory.h"
#include "schema.h"

// Arrow C Data Interface structs (Apache Arrow specification)
#ifndef ARROW_C_DATA_INTERFACE
#define ARROW_C_DATA_INTERFACE

struct ArrowSchema {
    const char* format;
    const char* name;
    const char* metadata;
    int64_t flags;
    int64_t n_children;
    struct ArrowSchema** children;
    struct ArrowSchema* dictionary;
    void (*release)(struct ArrowSchema*);
    void* private_data;
};

struct ArrowArray {
    int64_t length;
    int64_t null_count;
    int64_t offset;
    int64_t n_buffers;
    int64_t n_children;
    const void** buffers;
    struct ArrowArray** children;
    struct ArrowArray* dictionary;
    void (*release)(struct ArrowArray*);
    void* private_data;
};

#endif // ARROW_C_DATA_INTERFACE

// Magic number for arrow shared memory header
#define ARROW_SHM_MAGIC 0xA770DA7A

// 64-byte alignment for column data buffers (cache-line friendly)
#define ARROW_BUFFER_ALIGN 64
#define ARROW_ALIGN_UP(x) (((x) + ARROW_BUFFER_ALIGN - 1) & ~((size_t)ARROW_BUFFER_ALIGN - 1))

// Per-column descriptor in shared memory
typedef struct arrow_column_desc {
    morloc_serial_type type;    // column element type
    uint64_t length;            // number of elements
    uint64_t null_count;
    uint32_t name_offset;       // offset from header start to name string
    uint16_t name_length;
    uint64_t data_offset;       // offset from header start, 64-byte aligned
    uint64_t data_size;         // buffer size in bytes
} arrow_column_desc_t;

// Header at the start of Arrow data in shared memory
typedef struct arrow_shm_header {
    uint32_t magic;
    uint32_t n_columns;
    uint64_t n_rows;
    uint64_t total_size;        // total allocation including header
    // followed by: arrow_column_desc_t columns[n_columns]
    // followed by: packed column name strings
    // followed by: 64-byte aligned column data buffers
} arrow_shm_header_t;

#ifdef __cplusplus
extern "C" {
#endif

// Get element size in bytes for a morloc serial type (0 for variable-width)
size_t arrow_element_size(morloc_serial_type type);

// Map morloc serial type to Arrow format string character
const char* arrow_format_string(morloc_serial_type type);

// Map Arrow format string to morloc serial type
morloc_serial_type arrow_format_to_type(const char* format);

// Copy Arrow column buffers into morloc shared memory.
// Returns relptr to the arrow_shm_header in shared memory.
relptr_t arrow_to_shm(
    const struct ArrowArray* array,
    const struct ArrowSchema* schema,
    ERRMSG
);

// Validate shared memory Arrow data against morloc Schema.
// Returns 0 on success, sets errmsg on failure.
int arrow_validate(
    const arrow_shm_header_t* header,
    const Schema* schema,
    ERRMSG
);

// Get pointer to column data buffer (zero-copy read)
const void* arrow_column_data(
    const arrow_shm_header_t* header,
    uint32_t col_index
);

// Get column descriptor
const arrow_column_desc_t* arrow_column_desc(
    const arrow_shm_header_t* header,
    uint32_t col_index
);

// Get column name (not null-terminated; use name_length from descriptor)
const char* arrow_column_name(
    const arrow_shm_header_t* header,
    uint32_t col_index
);

// Build ArrowSchema and ArrowArray structs from shared memory data.
// The caller must call release on both when done.
// Buffers point directly into shared memory (zero-copy).
int arrow_from_shm(
    const arrow_shm_header_t* header,
    struct ArrowSchema* out_schema,
    struct ArrowArray* out_array,
    ERRMSG
);

#ifdef __cplusplus
}
#endif

#endif // __MORLOC_ARROW_H__
