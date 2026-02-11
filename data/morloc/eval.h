#ifndef __MORLOC_EVAL_H__
#define __MORLOC_EVAL_H__

#include "schema.h"
#include "stdint.h"
#include "stdbool.h"

typedef struct argument_s {
    char* value;
    char** fields;
    char** default_fields;
    size_t size;
} argument_t;

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

#endif // __MORLOC_EVAL_H__
