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


bool schema_is_fixed_width(const Schema* schema);
size_t calculate_voidstar_size(const void* data, const Schema* schema, ERRMSG);

// The voidstar representation of variable length data
typedef struct Array {
  size_t size;
  relptr_t data;
} Array;

#endif // __MORLOC_SCHEMA_H__
