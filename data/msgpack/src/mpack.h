// This code is built from libmpack: https://github.com/libmpack/libmpack

#ifndef MPACK_CORE_H
#define MPACK_CORE_H

#ifndef MPACK_API
# define MPACK_API extern
#endif

#include <assert.h>
#include <limits.h>
#include <stddef.h>

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

#if UINT_MAX == 0xffffffff
typedef int mpack_sint32_t;
typedef unsigned int mpack_uint32_t;
#elif ULONG_MAX == 0xffffffff
typedef long mpack_sint32_t;
typedef unsigned long mpack_uint32_t;
#else
# error "can't find unsigned 32-bit integer type"
#endif

typedef struct mpack_value_s {
  mpack_uint32_t lo, hi;
} mpack_value_t;


enum {
  MPACK_OK = 0,
  MPACK_EOF = 1,
  MPACK_ERROR = 2
};

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
  mpack_uint32_t length;    /* Byte length for str/bin/ext/chunk/float/int/uint.
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
  mpack_uint32_t passthrough;
} mpack_tokbuf_t;

#define MPACK_TOKBUF_INITIAL_VALUE { { 0 }, { 0, 0, { { 0, 0 } } }, 0, 0, 0 }

MPACK_API void mpack_tokbuf_init(mpack_tokbuf_t *tb) FUNUSED FNONULL;
MPACK_API int mpack_read(  mpack_tokbuf_t *tb, const char **b, size_t *bl,       mpack_token_t *tok) FUNUSED FNONULL;
MPACK_API int mpack_write( mpack_tokbuf_t *tb,       char **b, size_t *bl, const mpack_token_t *tok) FUNUSED FNONULL;


#if ULLONG_MAX == 0xffffffffffffffff
typedef long long mpack_sintmax_t;
typedef unsigned long long mpack_uintmax_t;
#elif UINT64_MAX == 0xffffffffffffffff
typedef int64_t mpack_sintmax_t;
typedef uint64_t mpack_uintmax_t;
#else
typedef mpack_sint32_t mpack_sintmax_t;
typedef mpack_uint32_t mpack_uintmax_t;
#endif

#ifndef bool
# define bool unsigned
#endif

MPACK_API mpack_token_t mpack_pack_nil(void) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_boolean(unsigned v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_uint(mpack_uintmax_t v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_sint(mpack_sintmax_t v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_int32(int v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_float_compat(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_float_fast(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_number(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_chunk(const char *p, mpack_uint32_t l) FUNUSED FPURE FNONULL;
MPACK_API mpack_token_t mpack_pack_str(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_bin(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_ext(int type, mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_array(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_map(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API bool mpack_unpack_boolean(mpack_token_t t) FUNUSED FPURE;
MPACK_API mpack_uintmax_t mpack_unpack_uint(mpack_token_t t) FUNUSED FPURE;
MPACK_API mpack_sintmax_t mpack_unpack_sint(mpack_token_t t) FUNUSED FPURE;

MPACK_API int mpack_unpack_uint32(mpack_token_t t) FUNUSED FPURE;
MPACK_API int mpack_unpack_sint32(mpack_token_t t) FUNUSED FPURE;

MPACK_API double mpack_unpack_float_fast(mpack_token_t t) FUNUSED FPURE;
MPACK_API double mpack_unpack_float_compat(mpack_token_t t) FUNUSED FPURE;
MPACK_API double mpack_unpack_number(mpack_token_t t) FUNUSED FPURE;

/* The mpack_{pack,unpack}_float_fast functions should work in 99% of the
 * platforms. When compiling for a platform where floats don't use ieee754 as
 * the internal format, pass
 * -Dmpack_{pack,unpack}_float=mpack_{pack,unpack}_float_compat to the
 *  compiler.*/
#ifndef mpack_pack_float
# define mpack_pack_float mpack_pack_float_fast
#endif
#ifndef mpack_unpack_float
# define mpack_unpack_float mpack_unpack_float_fast
#endif

#endif /* MPACK_CORE_H */
