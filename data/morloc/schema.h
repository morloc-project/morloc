#ifndef __MORLOC_SCHEMA_H__
#define __MORLOC_SCHEMA_H__

#include "memory.h"

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
  MORLOC_TENSOR,
  MORLOC_STRING,
  MORLOC_ARRAY,
  MORLOC_TUPLE,
  MORLOC_MAP,
  MORLOC_OPTIONAL
} morloc_serial_type;

#define SCHEMA_NIL    'z'
#define SCHEMA_BOOL   'b'
#define SCHEMA_SINT   'i'
#define SCHEMA_UINT   'u'
#define SCHEMA_FLOAT  'f'
#define SCHEMA_STRING 's'
#define SCHEMA_ARRAY  'a'
#define SCHEMA_TENSOR 'T'
#define SCHEMA_TUPLE  't'
#define SCHEMA_MAP    'm'
#define SCHEMA_OPTIONAL '?'

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


bool schema_is_fixed_width(const Schema* schema);
size_t schema_alignment(const Schema* schema);
size_t calculate_voidstar_size(const void* data, const Schema* schema, ERRMSG);

// Get tensor ndim from schema (stored in offsets[0])
static inline size_t schema_tensor_ndim(const Schema* schema) {
    return schema->offsets ? schema->offsets[0] : 0;
}

// The voidstar representation of variable length data
typedef struct Array {
  size_t size;
  relptr_t data;
} Array;

// The voidstar representation of a dense N-dimensional tensor.
// Data is always row-major (C order). Shape and data buffers live
// after the header in the same allocation, pointed to by relptrs.
// ndim is redundantly stored (also in schema->size) for self-description.
typedef struct Tensor {
  size_t total_elements;    // product of all shape dimensions
  uint32_t device_type;     // reserved for GPU: 0 = CPU
  uint32_t device_id;       // reserved for GPU: 0
  relptr_t data;            // relptr to contiguous element data
  relptr_t shape;           // relptr to int64_t[ndim]
} Tensor;

#endif // __MORLOC_SCHEMA_H__
