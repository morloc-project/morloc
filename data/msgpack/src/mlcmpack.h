#ifndef __MPACK_H__
#define __MPACK_H__

// The following code is adapted from libmpack: https://github.com/libmpack/libmpack ///////////////

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
MPACK_API int mpack_read(mpack_tokbuf_t *tb, const char **b, size_t *bl,
    mpack_token_t *tok) FUNUSED FNONULL;
MPACK_API int mpack_write(mpack_tokbuf_t *tb, char **b, size_t *bl,
    const mpack_token_t *tok) FUNUSED FNONULL;


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
MPACK_API mpack_token_t mpack_pack_float_compat(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_float_fast(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_number(double v) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_chunk(const char *p, mpack_uint32_t l)
  FUNUSED FPURE FNONULL;
MPACK_API mpack_token_t mpack_pack_str(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_bin(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_ext(int type, mpack_uint32_t l)
  FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_array(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API mpack_token_t mpack_pack_map(mpack_uint32_t l) FUNUSED FPURE;
MPACK_API bool mpack_unpack_boolean(mpack_token_t t) FUNUSED FPURE;
MPACK_API mpack_uintmax_t mpack_unpack_uint(mpack_token_t t) FUNUSED FPURE;
MPACK_API mpack_sintmax_t mpack_unpack_sint(mpack_token_t t) FUNUSED FPURE;
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

#include <string.h>


#define UNUSED(p) (void)p;
#define ADVANCE(buf, buflen) ((*buflen)--, (unsigned char)*((*buf)++))
#define TLEN(val, range_start) ((mpack_uint32_t)(1 << (val - range_start)))
#ifndef MIN
# define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

static int mpack_rtoken(const char **buf, size_t *buflen,
    mpack_token_t *tok);
static int mpack_rpending(const char **b, size_t *nl, mpack_tokbuf_t *tb);
static int mpack_rvalue(mpack_token_type_t t, mpack_uint32_t l,
    const char **b, size_t *bl, mpack_token_t *tok);
static int mpack_rblob(mpack_token_type_t t, mpack_uint32_t l,
    const char **b, size_t *bl, mpack_token_t *tok);
static int mpack_wtoken(const mpack_token_t *tok, char **b, size_t *bl);
static int mpack_wpending(char **b, size_t *bl, mpack_tokbuf_t *tb);
static int mpack_wpint(char **b, size_t *bl, mpack_value_t v);
static int mpack_wnint(char **b, size_t *bl, mpack_value_t v);
static int mpack_wfloat(char **b, size_t *bl, const mpack_token_t *v);
static int mpack_wstr(char **buf, size_t *buflen, mpack_uint32_t len);
static int mpack_wbin(char **buf, size_t *buflen, mpack_uint32_t len);
static int mpack_wext(char **buf, size_t *buflen, int type,
    mpack_uint32_t len);
static int mpack_warray(char **buf, size_t *buflen, mpack_uint32_t len);
static int mpack_wmap(char **buf, size_t *buflen, mpack_uint32_t len);
static int mpack_w1(char **b, size_t *bl, mpack_uint32_t v);
static int mpack_w2(char **b, size_t *bl, mpack_uint32_t v);
static int mpack_w4(char **b, size_t *bl, mpack_uint32_t v);
static mpack_value_t mpack_byte(unsigned char b);
static int mpack_value(mpack_token_type_t t, mpack_uint32_t l,
    mpack_value_t v, mpack_token_t *tok);
static int mpack_blob(mpack_token_type_t t, mpack_uint32_t l, int et,
    mpack_token_t *tok);

MPACK_API void mpack_tokbuf_init(mpack_tokbuf_t *tokbuf)
{
  tokbuf->ppos = 0;
  tokbuf->plen = 0;
  tokbuf->passthrough = 0;
}

MPACK_API int mpack_read(mpack_tokbuf_t *tokbuf, const char **buf,
    size_t *buflen, mpack_token_t *tok)
{
  int status;
  size_t initial_ppos, ptrlen, advanced;
  const char *ptr, *ptr_save;
  assert(*buf && *buflen);

  if (tokbuf->passthrough) {
    /* pass data from str/bin/ext directly as a MPACK_TOKEN_CHUNK, adjusting
     * *buf and *buflen */
    tok->type = MPACK_TOKEN_CHUNK;
    tok->data.chunk_ptr = *buf;
    tok->length = MIN((mpack_uint32_t)*buflen, tokbuf->passthrough);
    tokbuf->passthrough -= tok->length;
    *buf += tok->length;
    *buflen -= tok->length;
    goto done;
  }

  initial_ppos = tokbuf->ppos;

  if (tokbuf->plen) {
    if (!mpack_rpending(buf, buflen, tokbuf)) {
      return MPACK_EOF;
    }
    ptr = tokbuf->pending;
    ptrlen = tokbuf->ppos;
  } else {
    ptr = *buf;
    ptrlen = *buflen;
  }

  ptr_save = ptr;

  if ((status = mpack_rtoken(&ptr, &ptrlen, tok))) {
    if (status != MPACK_EOF) return MPACK_ERROR;
    /* need more data */
    assert(!tokbuf->plen);
    /* read the remainder of *buf to tokbuf->pending so it can be parsed
     * later with more data. only required when tokbuf->plen == 0 or else
     * it would have been done already. */
    tokbuf->plen = tok->length + 1;
    assert(tokbuf->plen <= sizeof(tokbuf->pending));
    tokbuf->ppos = 0;
    status = mpack_rpending(buf, buflen, tokbuf);
    assert(!status);
    return MPACK_EOF;
  }

  advanced = (size_t)(ptr - ptr_save) - initial_ppos;
  tokbuf->plen = tokbuf->ppos = 0;
  *buflen -= advanced; 
  *buf += advanced;

  if (tok->type > MPACK_TOKEN_MAP) {
    tokbuf->passthrough = tok->length;
  }

done:
  return MPACK_OK;
}

MPACK_API int mpack_write(mpack_tokbuf_t *tokbuf, char **buf, size_t *buflen, const mpack_token_t *t)
{
  int status;
  char *ptr;
  size_t ptrlen;
  mpack_token_t tok = tokbuf->plen ? tokbuf->pending_tok : *t;
  assert(*buf && *buflen);

  if (tok.type == MPACK_TOKEN_CHUNK) {
    size_t written, pending, count;
    if (!tokbuf->plen) tokbuf->ppos = 0;
    written = tokbuf->ppos;
    pending = tok.length - written;
    count = MIN(pending, *buflen);
    memcpy(*buf, tok.data.chunk_ptr + written, count);
    *buf += count;
    *buflen -= count;
    tokbuf->ppos += count;
    tokbuf->plen = count == pending ? 0 : tok.length;
    if (count == pending) {
      return MPACK_OK;
    } else {
      tokbuf->pending_tok = tok;
      return MPACK_EOF;
    }
  }

  if (tokbuf->plen) return mpack_wpending(buf, buflen, tokbuf);

  if (*buflen < MPACK_MAX_TOKEN_LEN) {
    ptr = tokbuf->pending;
    ptrlen = sizeof(tokbuf->pending);
  } else {
    ptr = *buf;
    ptrlen = *buflen;
  }

  if ((status = mpack_wtoken(&tok, &ptr, &ptrlen))) return status;

  if (*buflen < MPACK_MAX_TOKEN_LEN) {
    size_t toklen = sizeof(tokbuf->pending) - ptrlen;
    size_t write_cnt = MIN(toklen, *buflen);
    memcpy(*buf, tokbuf->pending, write_cnt);
    *buf += write_cnt;
    *buflen -= write_cnt;
    if (write_cnt < toklen) {
      assert(!*buflen);
      tokbuf->plen = toklen;
      tokbuf->ppos = write_cnt;
      tokbuf->pending_tok = tok;
      return MPACK_EOF;
    }
  } else {
    *buflen -= (size_t)(ptr - *buf);
    *buf = ptr;
  }

  return MPACK_OK;
}

static int mpack_rtoken(const char **buf, size_t *buflen,
    mpack_token_t *tok)
{
  unsigned char t = ADVANCE(buf, buflen);
  if (t < 0x80) {
    /* positive fixint */
    return mpack_value(MPACK_TOKEN_UINT, 1, mpack_byte(t), tok);
  } else if (t < 0x90) {
    /* fixmap */
    return mpack_blob(MPACK_TOKEN_MAP, t & 0xf, 0, tok);
  } else if (t < 0xa0) {
    /* fixarray */
    return mpack_blob(MPACK_TOKEN_ARRAY, t & 0xf, 0, tok);
  } else if (t < 0xc0) {
    /* fixstr */
    return mpack_blob(MPACK_TOKEN_STR, t & 0x1f, 0, tok);
  } else if (t < 0xe0) {
    switch (t) {
      case 0xc0:  /* nil */
        return mpack_value(MPACK_TOKEN_NIL, 0, mpack_byte(0), tok);
      case 0xc2:  /* false */
        return mpack_value(MPACK_TOKEN_BOOLEAN, 1, mpack_byte(0), tok);
      case 0xc3:  /* true */
        return mpack_value(MPACK_TOKEN_BOOLEAN, 1, mpack_byte(1), tok);
      case 0xc4:  /* bin 8 */
      case 0xc5:  /* bin 16 */
      case 0xc6:  /* bin 32 */
        return mpack_rblob(MPACK_TOKEN_BIN, TLEN(t, 0xc4), buf, buflen, tok);
      case 0xc7:  /* ext 8 */
      case 0xc8:  /* ext 16 */
      case 0xc9:  /* ext 32 */
        return mpack_rblob(MPACK_TOKEN_EXT, TLEN(t, 0xc7), buf, buflen, tok);
      case 0xca:  /* float 32 */
      case 0xcb:  /* float 64 */
        return mpack_rvalue(MPACK_TOKEN_FLOAT, TLEN(t, 0xc8), buf, buflen, tok);
      case 0xcc:  /* uint 8 */
      case 0xcd:  /* uint 16 */
      case 0xce:  /* uint 32 */
      case 0xcf:  /* uint 64 */
        return mpack_rvalue(MPACK_TOKEN_UINT, TLEN(t, 0xcc), buf, buflen, tok);
      case 0xd0:  /* int 8 */
      case 0xd1:  /* int 16 */
      case 0xd2:  /* int 32 */
      case 0xd3:  /* int 64 */
        return mpack_rvalue(MPACK_TOKEN_SINT, TLEN(t, 0xd0), buf, buflen, tok);
      case 0xd4:  /* fixext 1 */
      case 0xd5:  /* fixext 2 */
      case 0xd6:  /* fixext 4 */
      case 0xd7:  /* fixext 8 */
      case 0xd8:  /* fixext 16 */
        if (*buflen == 0) {
          /* require only one extra byte for the type code */
          tok->length = 1;
          return MPACK_EOF;
        }
        tok->length = TLEN(t, 0xd4);
        tok->type = MPACK_TOKEN_EXT;
        tok->data.ext_type = ADVANCE(buf, buflen);
        return MPACK_OK;
      case 0xd9:  /* str 8 */
      case 0xda:  /* str 16 */
      case 0xdb:  /* str 32 */
        return mpack_rblob(MPACK_TOKEN_STR, TLEN(t, 0xd9), buf, buflen, tok);
      case 0xdc:  /* array 16 */
      case 0xdd:  /* array 32 */
        return mpack_rblob(MPACK_TOKEN_ARRAY, TLEN(t, 0xdb), buf, buflen, tok);
      case 0xde:  /* map 16 */
      case 0xdf:  /* map 32 */
        return mpack_rblob(MPACK_TOKEN_MAP, TLEN(t, 0xdd), buf, buflen, tok);
      default:
        return MPACK_ERROR;
    }
  } else {
    /* negative fixint */
    return mpack_value(MPACK_TOKEN_SINT, 1, mpack_byte(t), tok);
  }
}

static int mpack_rpending(const char **buf, size_t *buflen,
    mpack_tokbuf_t *state)
{
  size_t count;
  assert(state->ppos < state->plen);
  count = MIN(state->plen - state->ppos, *buflen);
  memcpy(state->pending + state->ppos, *buf, count);
  state->ppos += count;
  if (state->ppos < state->plen) {
    /* consume buffer since no token will be parsed yet. */
    *buf += *buflen;
    *buflen = 0;
    return 0;
  }
  return 1;
}

static int mpack_rvalue(mpack_token_type_t type, mpack_uint32_t remaining,
    const char **buf, size_t *buflen, mpack_token_t *tok)
{
  if (*buflen < remaining) {
    tok->length = remaining;
    return MPACK_EOF;
  }

  mpack_value(type, remaining, mpack_byte(0), tok);

  while (remaining) {
    mpack_uint32_t byte = ADVANCE(buf, buflen), byte_idx, byte_shift;
    byte_idx = (mpack_uint32_t)--remaining;
    byte_shift = (byte_idx % 4) * 8;
    tok->data.value.lo |= byte << byte_shift;
    if (remaining == 4) {
      /* unpacked the first half of a 8-byte value, shift what was parsed to the
       * "hi" field and reset "lo" for the trailing 4 bytes. */
      tok->data.value.hi = tok->data.value.lo;
      tok->data.value.lo = 0;
    }
  }

  if (type == MPACK_TOKEN_SINT) {
    mpack_uint32_t hi = tok->data.value.hi;
    mpack_uint32_t lo = tok->data.value.lo;
    mpack_uint32_t msb = (tok->length == 8 && hi >> 31) ||
                         (tok->length == 4 && lo >> 31) ||
                         (tok->length == 2 && lo >> 15) ||
                         (tok->length == 1 && lo >> 7);
    if (!msb) {
      tok->type = MPACK_TOKEN_UINT;
    }
  }

  return MPACK_OK;
}

static int mpack_rblob(mpack_token_type_t type, mpack_uint32_t tlen,
    const char **buf, size_t *buflen, mpack_token_t *tok)
{
  mpack_token_t l;
  mpack_uint32_t required = tlen + (type == MPACK_TOKEN_EXT ? 1 : 0);

  if (*buflen < required) {
    tok->length = required;
    return MPACK_EOF;
  }

  l.data.value.lo = 0;
  mpack_rvalue(MPACK_TOKEN_UINT, tlen, buf, buflen, &l);
  tok->type = type;
  tok->length = l.data.value.lo;

  if (type == MPACK_TOKEN_EXT) {
    tok->data.ext_type = ADVANCE(buf, buflen);
  }

  return MPACK_OK;
}

static int mpack_wtoken(const mpack_token_t *tok, char **buf,
    size_t *buflen)
{
  switch (tok->type) {
    case MPACK_TOKEN_NIL:
      return mpack_w1(buf, buflen, 0xc0);
    case MPACK_TOKEN_BOOLEAN:
      return mpack_w1(buf, buflen, tok->data.value.lo ? 0xc3 : 0xc2);
    case MPACK_TOKEN_UINT:
      return mpack_wpint(buf, buflen, tok->data.value);
    case MPACK_TOKEN_SINT:
      return mpack_wnint(buf, buflen, tok->data.value);
    case MPACK_TOKEN_FLOAT:
      return mpack_wfloat(buf, buflen, tok);
    case MPACK_TOKEN_BIN:
      return mpack_wbin(buf, buflen, tok->length);
    case MPACK_TOKEN_STR:
      return mpack_wstr(buf, buflen, tok->length);
    case MPACK_TOKEN_EXT:
      return mpack_wext(buf, buflen, tok->data.ext_type, tok->length);
    case MPACK_TOKEN_ARRAY:
      return mpack_warray(buf, buflen, tok->length);
    case MPACK_TOKEN_MAP:
      return mpack_wmap(buf, buflen, tok->length);
    default:
      return MPACK_ERROR;
  }
}

static int mpack_wpending(char **buf, size_t *buflen, mpack_tokbuf_t *state)
{
  size_t count;
  assert(state->ppos < state->plen);
  count = MIN(state->plen - state->ppos, *buflen);
  memcpy(*buf, state->pending + state->ppos, count);
  state->ppos += count;
  *buf += count;
  *buflen -= count;
  if (state->ppos == state->plen) {
    state->plen = 0;
    return MPACK_OK;
  }
  return MPACK_EOF;
}

static int mpack_wpint(char **buf, size_t *buflen, mpack_value_t val)
{
  mpack_uint32_t hi = val.hi;
  mpack_uint32_t lo = val.lo;

  if (hi) {
    /* uint 64 */
    return mpack_w1(buf, buflen, 0xcf) ||
           mpack_w4(buf, buflen, hi)   ||
           mpack_w4(buf, buflen, lo);
  } else if (lo > 0xffff) {
    /* uint 32 */
    return mpack_w1(buf, buflen, 0xce) ||
           mpack_w4(buf, buflen, lo);
  } else if (lo > 0xff) {
    /* uint 16 */
    return mpack_w1(buf, buflen, 0xcd) ||
           mpack_w2(buf, buflen, lo);
  } else if (lo > 0x7f) {
    /* uint 8 */
    return mpack_w1(buf, buflen, 0xcc) ||
           mpack_w1(buf, buflen, lo);
  } else {
    return mpack_w1(buf, buflen, lo);
  }
}

static int mpack_wnint(char **buf, size_t *buflen, mpack_value_t val)
{
  mpack_uint32_t hi = val.hi;
  mpack_uint32_t lo = val.lo;

  if (lo <= 0x80000000) {
    /* int 64 */
    return mpack_w1(buf, buflen, 0xd3) ||
           mpack_w4(buf, buflen, hi)   ||
           mpack_w4(buf, buflen, lo);
  } else if (lo <= 0xffff7fff) {
    /* int 32 */
    return mpack_w1(buf, buflen, 0xd2) ||
           mpack_w4(buf, buflen, lo);
  } else if (lo <= 0xffffff7f) {
    /* int 16 */
    return mpack_w1(buf, buflen, 0xd1) ||
           mpack_w2(buf, buflen, lo);
  } else if (lo <= 0xffffffe0) {
    /* int 8 */
    return mpack_w1(buf, buflen, 0xd0) ||
           mpack_w1(buf, buflen, lo);
  } else {
    /* negative fixint */
    return mpack_w1(buf, buflen, (mpack_uint32_t)(0x100 + lo));
  }
}

static int mpack_wfloat(char **buf, size_t *buflen,
    const mpack_token_t *tok)
{
  if (tok->length == 4) {
    return mpack_w1(buf, buflen, 0xca) ||
           mpack_w4(buf, buflen, tok->data.value.lo);
  } else if (tok->length == 8) {
    return mpack_w1(buf, buflen, 0xcb) ||
           mpack_w4(buf, buflen, tok->data.value.hi) ||
           mpack_w4(buf, buflen, tok->data.value.lo);
  } else {
    return MPACK_ERROR;
  }
}

static int mpack_wstr(char **buf, size_t *buflen, mpack_uint32_t len)
{
  if (len < 0x20) {
    return mpack_w1(buf, buflen, 0xa0 | len);
  } else if (len < 0x100) {
    return mpack_w1(buf, buflen, 0xd9) ||
           mpack_w1(buf, buflen, len);
  } else if (len < 0x10000) {
    return mpack_w1(buf, buflen, 0xda) ||
           mpack_w2(buf, buflen, len);
  } else {
    return mpack_w1(buf, buflen, 0xdb) ||
           mpack_w4(buf, buflen, len);
  }
}

static int mpack_wbin(char **buf, size_t *buflen, mpack_uint32_t len)
{
  if (len < 0x100) {
    return mpack_w1(buf, buflen, 0xc4) ||
           mpack_w1(buf, buflen, len);
  } else if (len < 0x10000) {
    return mpack_w1(buf, buflen, 0xc5) ||
           mpack_w2(buf, buflen, len);
  } else {
    return mpack_w1(buf, buflen, 0xc6) ||
           mpack_w4(buf, buflen, len);
  }
}

static int mpack_wext(char **buf, size_t *buflen, int type,
    mpack_uint32_t len)
{
  mpack_uint32_t t;
  assert(type >= 0 && type < 0x80);
  t = (mpack_uint32_t)type;
  switch (len) {
    case 1: mpack_w1(buf, buflen, 0xd4); return mpack_w1(buf, buflen, t);
    case 2: mpack_w1(buf, buflen, 0xd5); return mpack_w1(buf, buflen, t);
    case 4: mpack_w1(buf, buflen, 0xd6); return mpack_w1(buf, buflen, t);
    case 8: mpack_w1(buf, buflen, 0xd7); return mpack_w1(buf, buflen, t);
    case 16: mpack_w1(buf, buflen, 0xd8); return mpack_w1(buf, buflen, t);
    default:
      if (len < 0x100) {
        return mpack_w1(buf, buflen, 0xc7) ||
               mpack_w1(buf, buflen, len)  ||
               mpack_w1(buf, buflen, t);
      } else if (len < 0x10000) {
        return mpack_w1(buf, buflen, 0xc8) ||
               mpack_w2(buf, buflen, len)  ||
               mpack_w1(buf, buflen, t);
      } else {
        return mpack_w1(buf, buflen, 0xc9) ||
               mpack_w4(buf, buflen, len)  ||
               mpack_w1(buf, buflen, t);
      }
  }
}

static int mpack_warray(char **buf, size_t *buflen, mpack_uint32_t len)
{
  if (len < 0x10) {
    return mpack_w1(buf, buflen, 0x90 | len);
  } else if (len < 0x10000) {
    return mpack_w1(buf, buflen, 0xdc) ||
           mpack_w2(buf, buflen, len);
  } else {
    return mpack_w1(buf, buflen, 0xdd) ||
           mpack_w4(buf, buflen, len);
  }
}

static int mpack_wmap(char **buf, size_t *buflen, mpack_uint32_t len)
{
  if (len < 0x10) {
    return mpack_w1(buf, buflen, 0x80 | len);
  } else if (len < 0x10000) {
    return mpack_w1(buf, buflen, 0xde) ||
           mpack_w2(buf, buflen, len);
  } else {
    return mpack_w1(buf, buflen, 0xdf) ||
           mpack_w4(buf, buflen, len);
  }
}

static int mpack_w1(char **b, size_t *bl, mpack_uint32_t v)
{
  (*bl)--;
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_w2(char **b, size_t *bl, mpack_uint32_t v)
{
  *bl -= 2;
  *(*b)++ = (char)((v >> 8) & 0xff);
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_w4(char **b, size_t *bl, mpack_uint32_t v)
{
  *bl -= 4;
  *(*b)++ = (char)((v >> 24) & 0xff);
  *(*b)++ = (char)((v >> 16) & 0xff);
  *(*b)++ = (char)((v >> 8) & 0xff);
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_value(mpack_token_type_t type, mpack_uint32_t length,
    mpack_value_t value, mpack_token_t *tok)
{
  tok->type = type;
  tok->length = length;
  tok->data.value = value;
  return MPACK_OK;
}

static int mpack_blob(mpack_token_type_t type, mpack_uint32_t length,
    int ext_type, mpack_token_t *tok)
{
  tok->type = type;
  tok->length = length;
  tok->data.ext_type = ext_type;
  return MPACK_OK;
}

static mpack_value_t mpack_byte(unsigned char byte)
{
  mpack_value_t rv;
  rv.lo = byte;
  rv.hi = 0;
  return rv;
}

static int mpack_fits_single(double v);
static mpack_value_t mpack_pack_ieee754(double v, unsigned m, unsigned e);
static int mpack_is_be(void) FPURE;
static double mpack_fmod_pow2_32(double a);


#define POW2(n) \
  ((double)(1 << (n / 2)) * (double)(1 << (n / 2)) * (double)(1 << (n % 2)))

#define MPACK_SWAP_VALUE(val)                                  \
  do {                                                         \
    mpack_uint32_t lo = val.lo;                                \
    val.lo = val.hi;                                           \
    val.hi = lo;                                               \
  } while (0)

MPACK_API mpack_token_t mpack_pack_nil(void)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_NIL;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_boolean(unsigned v)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_BOOLEAN;
  rv.data.value.lo = v ? 1 : 0;
  rv.data.value.hi = 0;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_uint(mpack_uintmax_t v)
{
  mpack_token_t rv;
  rv.data.value.lo = v & 0xffffffff;
  rv.data.value.hi = (mpack_uint32_t)((v >> 31) >> 1);
  rv.type = MPACK_TOKEN_UINT;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_sint(mpack_sintmax_t v)
{
  if (v < 0) {
    mpack_token_t rv;
    mpack_uintmax_t tc = -((mpack_uintmax_t)(v + 1)) + 1;
    tc = ~tc + 1;
    rv = mpack_pack_uint(tc);
    rv.type = MPACK_TOKEN_SINT;
    return rv;
  }

  return mpack_pack_uint((mpack_uintmax_t)v);
}

MPACK_API mpack_token_t mpack_pack_int32(int v){
  mpack_token_t rv;
  rv.data.value.lo = v;
  rv.data.value.hi = 0;
  if (v >= 0){
    rv.type = MPACK_TOKEN_UINT;
  } else {
    rv.type = MPACK_TOKEN_SINT;
  }
  return rv;
}

MPACK_API mpack_token_t mpack_pack_float_compat(double v)
{
  /* ieee754 single-precision limits to determine if "v" can be fully
   * represented in 4 bytes */
  mpack_token_t rv;

  if (mpack_fits_single(v)) {
    rv.length = 4;
    rv.data.value = mpack_pack_ieee754(v, 23, 8);
  } else {
    rv.length = 8;
    rv.data.value = mpack_pack_ieee754(v, 52, 11);
  }

  rv.type = MPACK_TOKEN_FLOAT;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_float_fast(double v)
{
  /* ieee754 single-precision limits to determine if "v" can be fully
   * represented in 4 bytes */
  mpack_token_t rv;

  if (mpack_fits_single(v)) {
    union {
      float f;
      mpack_uint32_t m;
    } conv;
    conv.f = (float)v;
    rv.length = 4;
    rv.data.value.lo = conv.m;
    rv.data.value.hi = 0;
  } else {
    union {
      double d;
      mpack_value_t m;
    } conv;
    conv.d = v;
    rv.length = 8;
    rv.data.value = conv.m;
    if (mpack_is_be()) {
      MPACK_SWAP_VALUE(rv.data.value);
    }
  }

  rv.type = MPACK_TOKEN_FLOAT;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_number(double v)
{
  mpack_token_t tok;
  double vabs;
  vabs = v < 0 ? -v : v;
  assert(v <= 9007199254740991. && v >= -9007199254740991.);
  tok.data.value.hi = (mpack_uint32_t)(vabs / POW2(32));
  tok.data.value.lo = (mpack_uint32_t)mpack_fmod_pow2_32(vabs);

  if (v < 0) {
    /* Compute the two's complement */
    tok.type = MPACK_TOKEN_SINT;
    tok.data.value.hi = ~tok.data.value.hi;
    tok.data.value.lo = ~tok.data.value.lo + 1;
    if (!tok.data.value.lo) tok.data.value.hi++;
    if (tok.data.value.lo == 0 && tok.data.value.hi == 0) tok.length = 1;
    else if (tok.data.value.lo < 0x80000000) tok.length = 8;
    else if (tok.data.value.lo < 0xffff7fff) tok.length = 4;
    else if (tok.data.value.lo < 0xffffff7f) tok.length = 2;
    else tok.length = 1;
  } else {
    tok.type = MPACK_TOKEN_UINT;
    if (tok.data.value.hi) tok.length = 8;
    else if (tok.data.value.lo > 0xffff) tok.length = 4;
    else if (tok.data.value.lo > 0xff) tok.length = 2;
    else tok.length = 1;
  }

  if (mpack_unpack_number(tok) != v) {
    return mpack_pack_float(v);
  }

  return tok;
}

MPACK_API mpack_token_t mpack_pack_chunk(const char *p, mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_CHUNK;
  rv.data.chunk_ptr = p;
  rv.length = l;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_str(mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_STR;
  rv.length = l;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_bin(mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_BIN;
  rv.length = l;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_ext(int t, mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_EXT;
  rv.length = l;
  rv.data.ext_type = t;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_array(mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_ARRAY;
  rv.length = l;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_map(mpack_uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_MAP;
  rv.length = l;
  return rv;
}

MPACK_API bool mpack_unpack_boolean(mpack_token_t t)
{
  return t.data.value.lo || t.data.value.hi;
}

MPACK_API mpack_uintmax_t mpack_unpack_uint(mpack_token_t t)
{
  return (((mpack_uintmax_t)t.data.value.hi << 31) << 1) | t.data.value.lo;
}

MPACK_API int mpack_unpack_uint32(mpack_token_t t)
{
  return (int) t.data.value.lo;
}

/* unpack signed integer without relying on two's complement as internal
 * representation */
MPACK_API mpack_sintmax_t mpack_unpack_sint(mpack_token_t t)
{
  mpack_uint32_t hi = t.data.value.hi;
  mpack_uint32_t lo = t.data.value.lo;
  mpack_uintmax_t rv = lo;
  // This assert fails because length is not initialized
  // The problem has probably gone undetected because asserts disappear with -O
  /* assert(t.length <= sizeof(mpack_sintmax_t)); */

  if (t.length == 8) {
    rv |= (((mpack_uintmax_t)hi) << 31) << 1;
  }

  /* reverse the two's complement so that lo/hi contain the absolute value.
   * note that we have to mask ~rv so that it reflects the two's complement
   * of the appropriate byte length */
  rv = (~rv & (((mpack_uintmax_t)1 << ((t.length * 8) - 1)) - 1)) + 1;
  /* negate and return the absolute value, making sure mpack_sintmax_t can
   * represent the positive cast. */
  return -((mpack_sintmax_t)(rv - 1)) - 1;
}

MPACK_API int mpack_unpack_sint32(mpack_token_t t)
{
  int rv = (int)t.data.value.lo;
  rv = ~rv + 1;
  return -(rv - 1) - 1;
}


MPACK_API double mpack_unpack_float_compat(mpack_token_t t)
{
  mpack_uint32_t sign;
  mpack_sint32_t exponent, bias;
  unsigned mantbits;
  unsigned expbits;
  double mant;

  if (t.data.value.lo == 0 && t.data.value.hi == 0)
    /* nothing to do */
    return 0;

  if (t.length == 4) mantbits = 23, expbits = 8;
  else mantbits = 52, expbits = 11;
  bias = (1 << (expbits - 1)) - 1;

  /* restore sign/exponent/mantissa */
  if (mantbits == 52) {
    sign = t.data.value.hi >> 31;
    exponent = (t.data.value.hi >> 20) & ((1 << 11) - 1);
    mant = (t.data.value.hi & ((1 << 20) - 1)) * POW2(32);
    mant += t.data.value.lo;
  } else {
    sign = t.data.value.lo >> 31;
    exponent = (t.data.value.lo >> 23) & ((1 << 8) - 1);
    mant = t.data.value.lo & ((1 << 23) - 1);
  }

  mant /= POW2(mantbits);
  if (exponent) mant += 1.0; /* restore leading 1 */
  else exponent = 1; /* subnormal */
  exponent -= bias;

  /* restore original value */
  while (exponent > 0) mant *= 2.0, exponent--;
  while (exponent < 0) mant /= 2.0, exponent++;
  return mant * (sign ? -1 : 1);
}

MPACK_API double mpack_unpack_float_fast(mpack_token_t t)
{
  if (t.length == 4) {
    union {
      float f;
      mpack_uint32_t m;
    } conv;
    conv.m = t.data.value.lo;
    return conv.f;
  } else {
    union {
      double d;
      mpack_value_t m;
    } conv;
    conv.m = t.data.value;
    
    if (mpack_is_be()) {
      MPACK_SWAP_VALUE(conv.m);
    }

    return conv.d;
  }
}

MPACK_API double mpack_unpack_number(mpack_token_t t)
{
  double rv;
  mpack_uint32_t hi, lo;
  if (t.type == MPACK_TOKEN_FLOAT) return mpack_unpack_float(t);
  assert(t.type == MPACK_TOKEN_UINT || t.type == MPACK_TOKEN_SINT);
  hi = t.data.value.hi;
  lo = t.data.value.lo;
  if (t.type == MPACK_TOKEN_SINT) {
    /* same idea as mpack_unpack_sint, except here we shouldn't rely on
     * mpack_uintmax_t having 64-bits, operating on the 32-bit words separately.
     */
    if (!hi) {
      assert(t.length <= 4);
      hi = 0;
      lo = (~lo & (((mpack_uint32_t)1 << ((t.length * 8) - 1)) - 1));
    } else {
      hi = ~hi;
      lo = ~lo;
    }
    lo++;
    if (!lo) hi++;
  }
  rv = (double)lo + POW2(32) * hi;
  return t.type == MPACK_TOKEN_SINT ? -rv : rv;
}

static int mpack_fits_single(double v)
{
  return (float)v == v;
}

static mpack_value_t mpack_pack_ieee754(double v, unsigned mantbits,
    unsigned expbits)
{
  mpack_value_t rv = {0, 0};
  mpack_sint32_t exponent, bias = (1 << (expbits - 1)) - 1;
  mpack_uint32_t sign;
  double mant;

  if (v == 0) {
    rv.lo = 0;
    rv.hi = 0;
    goto end;
  }

  if (v < 0) sign = 1, mant = -v;
  else sign = 0, mant = v;

  exponent = 0;
  while (mant >= 2.0) mant /= 2.0, exponent++;
  while (mant < 1.0 && exponent > -(bias - 1)) mant *= 2.0, exponent--;

  if (mant < 1.0) exponent = -bias; /* subnormal value */
  else mant = mant - 1.0; /* remove leading 1 */
  exponent += bias;
  mant *= POW2(mantbits);

  if (mantbits == 52) {
    rv.hi = (mpack_uint32_t)(mant / POW2(32));
    rv.lo = (mpack_uint32_t)(mant - rv.hi * POW2(32));
    rv.hi |= ((mpack_uint32_t)exponent << 20) | (sign << 31);
  } else if (mantbits == 23) {
    rv.hi = 0;
    rv.lo = (mpack_uint32_t)mant;
    rv.lo |= ((mpack_uint32_t)exponent << 23) | (sign << 31);
  }

end:
  return rv;
}

static int mpack_is_be(void)
{
  union {
    mpack_uint32_t i;
    char c[sizeof(mpack_uint32_t)];
  } test;

  test.i = 1;
  return test.c[0] == 0;
}

/* this simplified version of `fmod` that returns the remainder of double
 * division by 0xffffffff, which is enough for our purposes */
static double mpack_fmod_pow2_32(double a)
{
  return a - ((double)(mpack_uint32_t)(a / POW2(32)) * POW2(32));
}


#endif // end __MPACK_H__





#ifndef __MLCMPACK_HEADER_ONLY_H__
#define __MLCMPACK_HEADER_ONLY_H__

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>

// Forward declarations
struct Schema;
struct Anything;

typedef enum {
  MORLOC_NIL           =  0,
  MORLOC_BOOL          =  1,
  MORLOC_INT           =  2,
  MORLOC_FLOAT         =  3,
  MORLOC_STRING        =  4,
  MORLOC_BINARY        =  5,
  MORLOC_ARRAY         =  6,
  MORLOC_MAP           =  7,
  MORLOC_TUPLE         =  8,
  MORLOC_BOOL_ARRAY    =  9,
  MORLOC_INT_ARRAY     =  10,
  MORLOC_FLOAT_ARRAY   =  11,
  MORLOC_EXT           =  12
} morloc_serial_type;

#define SCHEMA_ARRAY  'a'
#define SCHEMA_TUPLE  't'
#define SCHEMA_MAP    'm'
#define SCHEMA_NIL    'z'
#define SCHEMA_BOOL   'b'
#define SCHEMA_SINT   'i'
#define SCHEMA_UINT   'u'
#define SCHEMA_FLOAT  'f'
#define SCHEMA_STRING 's'
#define SCHEMA_BINARY 'r'

#define BUFFER_SIZE 4096

// The maximum nesting depth of a data structure. This should be deep enough for
// any non-recursive datastructure. For recursive structures, trees and such, I
// will make a dedicated function that does not depend on this limit. This
// function would use a linked list for the stack instead of an array. The
// downside of the linked list is that it must live on the heap and will be
// slower.
#define MAX_DEPTH 128

// Schema definition
//  * Primitives have no parameters
//  * Arrays have one
//  * Tuples and records have one or more
typedef struct Schema {
    morloc_serial_type type;
    size_t size; // number of parameters
    struct Schema** parameters;
    char** keys; // field names, used only for records
} Schema;

// A data structure that stores anything representable by MessagePack
// The morloc pools will need to transform their data to/from this form
typedef struct Anything {
    morloc_serial_type type;
    size_t size; // 0 for primitives, array length for containers
    char* key; // NULL terminated string, used for names of elements in maps
    union {
        char nil_val; // set to 0x00, but the actual value will not be used
        bool bool_val;
        int int_val;
        double double_val;
        char* char_arr; // bytes, strings, and extensions
        bool* bool_arr;
        int* int_arr;
        double* float_arr;
        struct Anything** obj_arr; // general arrays, tuples, and maps
    } data;
} Anything;

// Prototypes

void free_parsed_data(Anything* data);
void free_schema(Schema* schema);

Schema* parse_schema(const char** schema_ptr);

void print_parsed_data(const Anything* data);
void print_schema(const Schema* schema);

// Main pack function for creating morloc-encoded MessagePack data
int pack(const Anything* data, const char* schema_str, char** out_data, size_t* out_size);
int pack_with_schema(const Anything* data, const Schema* schema, char** out_data, size_t* out_size);

int unpack(const char* data, size_t data_size, const char* schema_str, Anything** out_data);
int unpack_with_schema(const char* data, size_t data_size, const Schema* schema, Anything** out_data);


// create atomic values
Anything* nil_data();
Anything* bool_data(bool value);
Anything* int_data(int value);
Anything* float_data(double value);
// strings and binary
Anything* string_data(const char* value, size_t size);
Anything* binary_data(const char* value, size_t size);
Anything* string_data_(size_t size);
Anything* binary_data_(size_t size);
// unboxed arrays
Anything* array_bool_data(const bool* values, size_t size);
Anything* array_int_data(const int* values, size_t size);
Anything* array_float_data(const double* values, size_t size);
// unboxed uninitialized arrays
Anything* array_bool_data_(size_t size);
Anything* array_int_data_(size_t size);
Anything* array_float_data_(size_t size);
// containers, set elements individually
Anything* array_data_(size_t size);
Anything* tuple_data_(size_t size);
Anything* map_data_(size_t size);
// helper for setting map elements
void set_map_element(Anything* map, size_t pos, const char* key, Anything* value);

void print_hex(const char* data, size_t size);
void write_tokens(const char** buf_ptr, size_t* buf_remaining);


void print_hex(const char* data, size_t size) {
    for (size_t i = 0; i < size; i++) {
        fprintf(stderr, "%02x", (unsigned char)data[i]);
        if (i < size - 1) {
            fprintf(stderr, " ");
        }
    }
}

// Helper function to create a schema with parameters
Schema* create_schema_with_params(morloc_serial_type type, int size, Schema** params, char** keys) {
    Schema* schema = (Schema*)malloc(sizeof(Schema));
    if (!schema) return NULL;

    schema->type = type;
    schema->size = size;
    schema->parameters = params;
    schema->keys = keys;

    return schema;
}

Schema* nil_schema() {
    return create_schema_with_params(MORLOC_NIL, 0, NULL, NULL);
}

Schema* bool_schema() {
    return create_schema_with_params(MORLOC_BOOL, 0, NULL, NULL);
}

Schema* int_schema() {
    return create_schema_with_params(MORLOC_INT, 0, NULL, NULL);
}

Schema* float_schema() {
    return create_schema_with_params(MORLOC_FLOAT, 0, NULL, NULL);
}

Schema* string_schema() {
    return create_schema_with_params(MORLOC_STRING, 0, NULL, NULL);
}

Schema* binary_schema() {
    return create_schema_with_params(MORLOC_BINARY, 0, NULL, NULL);
}

Schema* bool_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = bool_schema();
    return create_schema_with_params(MORLOC_BOOL_ARRAY, 1, params, NULL);
}

Schema* int_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = int_schema();
    return create_schema_with_params(MORLOC_INT_ARRAY, 1, params, NULL);
}

Schema* float_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = float_schema();
    return create_schema_with_params(MORLOC_FLOAT_ARRAY, 1, params, NULL);
}

Schema* tuple_schema_(size_t size) {

    Schema** params = (Schema**)calloc(size, sizeof(Schema*));
    if (!params) {
        return NULL;
    }

    return create_schema_with_params(MORLOC_TUPLE, size, params, NULL);
}

Schema* array_schema(Schema* array_type) {
    switch(array_type->type){
      case MORLOC_BOOL:
        return bool_array_schema();
      case MORLOC_INT:
        return int_array_schema();
      case MORLOC_FLOAT:
        return float_array_schema();
      default:
        Schema** params = (Schema**)malloc(sizeof(Schema*));
        if (!params) return NULL;
        
        params[0] = array_type;
        
        return create_schema_with_params(MORLOC_ARRAY, 1, params, NULL);
    }
}

Schema* map_schema_(size_t size) {
    Schema** params = (Schema**)calloc(size, sizeof(Schema*));
    char** keys = (char**)calloc(size, sizeof(char*));

    if (!params || !keys) {
        free(params);
        free(keys);
        return NULL;
    }

    return create_schema_with_params(MORLOC_MAP, size, params, keys);
}

// Free a Schema and its contents
void free_schema(Schema* schema) {
    if (!schema) return;

    if (schema->parameters) {
        for (int i = 0; i < schema->size; i++) {
            if(schema->parameters[i]){
              free_schema(schema->parameters[i]);
            }
        }
        free(schema->parameters);
    }

    if (schema->keys) {
        for (int i = 0; i < schema->size; i++) {
            if (schema->keys[i]) {
                free(schema->keys[i]);
            }
        }
        free(schema->keys);
    }

    free(schema);
}



// Helper function to create a Anything with allocated memory
Anything* create_parsed_data(morloc_serial_type type, size_t size) {
    Anything* data = (Anything*)malloc(sizeof(Anything));
    if (!data) return NULL;
    data->type = type;
    data->size = size;
    data->key = NULL;
    return data;
}

// Helper functions for primitive types
Anything* nil_data() {
    Anything* data = create_parsed_data(MORLOC_NIL, 0);
    // I won't actually use this value, so I keep the msgpck value
    if (data) data->data.nil_val = 0xc0;
    return data;
}

Anything* bool_data(bool value) {
    Anything* data = create_parsed_data(MORLOC_BOOL, 0);
    if (data) data->data.bool_val = value;
    return data;
}

Anything* int_data(int value) {
    Anything* data = create_parsed_data(MORLOC_INT, 0);
    if (data) data->data.int_val = value;
    return data;
}

Anything* float_data(double value) {
    Anything* data = create_parsed_data(MORLOC_FLOAT, 0);
    if (data) data->data.double_val = value;
    return data;
}


Anything* char_data_(size_t size, morloc_serial_type mtype) {
    Anything* data = create_parsed_data(mtype, size);
    if (data) {
        data->data.char_arr = (char*)calloc(size, sizeof(char));
        if (!data->data.char_arr) {
            free(data);
            return NULL;
        }
    }
    return data;
}

Anything* string_data_(size_t size) {
    return char_data_(size, MORLOC_STRING);
}

Anything* binary_data_(size_t size) {
    return char_data_(size, MORLOC_BINARY);
}

Anything* ext_data_(size_t size, int ext_type) {
    // the ext_type is ignored since no support is currently offered
    return char_data_(size, MORLOC_EXT);
}


Anything* obj_array_data_(size_t size, morloc_serial_type mtype) {
    Anything* data = create_parsed_data(mtype, size);
    if (data) {
        data->data.obj_arr = (Anything**)calloc(size, sizeof(Anything*));
        if (!data->data.obj_arr) {
            free(data);
            return NULL;
        }
    }
    return data;
}

// Helper function for arrays
Anything* array_data_(size_t size) {
    return obj_array_data_(size, MORLOC_ARRAY);
}

// Helper function for tuples
Anything* tuple_data_(size_t size) {
    return obj_array_data_(size, MORLOC_TUPLE);
}

// Helper function for maps
Anything* map_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_MAP, size);
    if (data) {
        data->data.obj_arr = (Anything**)calloc(size, sizeof(Anything*));
        if (!data->data.obj_arr) {
            free(data->data.obj_arr);
            free(data);
            return NULL;
        }
    }
    return data;
}

// Helper function to set a key-value pair in a map
void set_map_element(Anything* map, size_t pos, const char* key, Anything* value) {
    value->key = strdup(key);
    map->data.obj_arr[pos] = value;
}

Anything* array_bool_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_BOOL_ARRAY, size);
    data->data.bool_arr = (bool*)calloc(size, sizeof(bool));
    if (!data->data.bool_arr) {
        free(data);
        return NULL;
    }
    return data;
}

Anything* array_int_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_INT_ARRAY, size);
    data->data.int_arr = (int*)malloc(size * sizeof(int));
    if (!data->data.int_arr) {
        free(data);
        return NULL;
    }

    return data;
}

Anything* array_float_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_FLOAT_ARRAY, size);
    data->data.float_arr = (double*)malloc(size * sizeof(double));
    if (!data->data.float_arr) {
        free(data);
        return NULL;
    }

    return data;
}

// Helper function for array of booleans
Anything* array_bool_data(const bool* values, size_t size) {
    Anything* data = array_bool_data_(size);
    if (data) {
        memcpy(data->data.bool_arr, values, size * sizeof(bool));
    }
    return data;
}

// Helper function for array of signed integers
Anything* array_int_data(const int* values, size_t size) {
    Anything* data = array_int_data_(size);
    if (data) {
        memcpy(data->data.int_arr, values, size * sizeof(int));
    }
    return data;
}

// Helper function for array of floats (doubles)
Anything* array_float_data(const double* values, size_t size) {
    Anything* data = array_float_data_(size);
    if (data) {
        memcpy(data->data.float_arr, values, size * sizeof(double));
    }
    return data;
}

Anything* string_data(const char* value, size_t size) {
    Anything* data = string_data_(size);
    if (data) {
        memcpy(data->data.char_arr, value, size);
    }
    return data;
}

Anything* binary_data(const char* value, size_t size) {
    Anything* data = binary_data_(size);
    if (data) {
        memcpy(data->data.char_arr, value, size);
    }
    return data;
}


void free_parsed_data(Anything* data) {
    if (!data) return;

    switch (data->type) {
        case MORLOC_NIL:
            // No additional freeing needed
            break;
        case MORLOC_BOOL:
            // For a single boolean, no additional freeing needed
            break;
        case MORLOC_INT:
        case MORLOC_FLOAT:
            // For single primitives, no additional freeing needed
            break;
        case MORLOC_BINARY:
        case MORLOC_STRING:
            free(data->data.char_arr);
            break;
        case MORLOC_MAP:
        case MORLOC_ARRAY:
            if (data->data.obj_arr) {
                for (size_t i = 0; i < data->size; i++) {
                    free_parsed_data(data->data.obj_arr[i]);
                }
                free(data->data.obj_arr);
            }
            break;
        case MORLOC_BOOL_ARRAY:
            free(data->data.bool_arr);
            break;
        case MORLOC_INT_ARRAY:
            free(data->data.int_arr);
            break;
        case MORLOC_FLOAT_ARRAY:
            free(data->data.float_arr);
            break;
        default:
            // Unknown type, do nothing
            break;
    }

    free(data);
}

// packing ####


void upsize(
  char** data,            // data that will be resized
  char** data_ptr,        // pointer that will be updated to preserve offset
  size_t* remaining_size, // remaining data size
  size_t added_size       // the number of bytes that need to be added
){
    // check if any action is needed
    if (added_size <= *remaining_size) {
        return;
    }

    size_t used_size = *data_ptr - *data;
    size_t buffer_size = used_size + *remaining_size;

    // find an appropriate size for the new data
    while (added_size > *remaining_size) {
        if (buffer_size > SIZE_MAX / 2) {
            buffer_size += BUFFER_SIZE;
        } else {
            buffer_size *= 2;
        }
        *remaining_size = buffer_size - used_size;
    }

    // allocate memory for the new data
    *data = (char*)realloc(*data, buffer_size);

    // point old pointer to the same offset in the new data
    *data_ptr = *data + used_size;
}


void write_to_packet(
  void* src,                // source data
  char** packet,            // destination
  char** packet_ptr,        // location in the destination that will be written to
  size_t* packet_remaining, // remaining data size
  size_t size               // the number of bytes to write

){
    upsize(packet, packet_ptr, packet_remaining, size);
    memcpy(*packet_ptr, src, size);
    *packet_ptr += size;
    *packet_remaining -= size;
}


int dynamic_mpack_write(
  mpack_tokbuf_t* tokbuf,
  char** packet,
  char** packet_ptr,
  size_t* packet_remaining,
  mpack_token_t* token,
  size_t extra_size
) {
    int result = 0;
    if(*packet_remaining <= 0){
        upsize(packet, packet_ptr, packet_remaining, 1);
    }
    result = mpack_write(tokbuf, packet_ptr, packet_remaining, token);
    if (result == MPACK_EOF || *packet_remaining == 0) {
        upsize(packet, packet_ptr, packet_remaining, token->length + extra_size);
        if (result == MPACK_EOF) {
            mpack_write(tokbuf, packet_ptr, packet_remaining, token);
        }
    }
    return result;
}

size_t parse_schema_size(const char** schema_ptr){
  char c = **schema_ptr; 
  size_t size =
    // characters 0-9 are integers 0-9
    (c >= 0x30 && c <= 0x39) * (c - 0x30) +
    // characters a-z are integers 10-35
    (c >= 0x61 && c <= 0x7a) * (c - 0x61 + 10) +
    // characters A-Z are integers 36-61
    (c >= 0x41 && c <= 0x5a) * (c - 0x41 + 36) +
    // '+' is 62
    (c == '+') * 62 +
    // '/' is 63
    (c == '/') * 63;
  (*schema_ptr)++;
  return size;
}

char* parse_schema_key(const char** schema_ptr){
  size_t key_size = parse_schema_size(schema_ptr);
  char* key = (char*)calloc(key_size+1, sizeof(char));
  memcpy(key, *schema_ptr, key_size); 
  *schema_ptr += key_size;
  return key;
}

Schema* parse_schema(const char** schema_ptr){
  Schema* schema = 0;
  char c = **schema_ptr;
  (*schema_ptr)++;
  size_t size;

  switch(c){
    case SCHEMA_ARRAY:
      return array_schema(parse_schema(schema_ptr));
    case SCHEMA_TUPLE:
      size = parse_schema_size(schema_ptr);
      schema = tuple_schema_(size);
      for(size_t i = 0; i < size; i++){
        schema->parameters[i] = parse_schema(schema_ptr);
      }
      return schema;
    case SCHEMA_MAP:
      size = parse_schema_size(schema_ptr);
      schema = map_schema_(size);
      for(size_t i = 0; i < size; i++){
        schema->keys[i] = parse_schema_key(schema_ptr);
        schema->parameters[i] = parse_schema(schema_ptr);
      }
      return schema;
    case SCHEMA_NIL:
      return nil_schema();
    case SCHEMA_BOOL:
      return bool_schema();
    case SCHEMA_SINT:
      size = parse_schema_size(schema_ptr);
      return int_schema();
    case SCHEMA_UINT:
      size = parse_schema_size(schema_ptr);
      return int_schema();
    case SCHEMA_FLOAT:
      size = parse_schema_size(schema_ptr);
      return float_schema();
    case SCHEMA_STRING:
      return string_schema();
    case SCHEMA_BINARY:
      return binary_schema();
    default:
      fprintf(stderr, "Unrecognized schema type '%c'\n", c);
      return 0;
  }
}

//  The main function for writing MessagePack
int pack_data(
  const Anything* data,    // input data structure
  const Schema* schema,      // input data schema
  char** packet,             // a pointer to the messagepack data
  char** packet_ptr,         // the current position in the buffer
  size_t* packet_remaining,  // bytes from current position to the packet end
  mpack_tokbuf_t* tokbuf
) {
    mpack_token_t token;
    int result;

    switch (schema->type) {
        case MORLOC_NIL:
            token = mpack_pack_nil();
            break;
        case MORLOC_BOOL:
            token = mpack_pack_boolean(data->data.bool_val);
            break;
        case MORLOC_INT:
            token = mpack_pack_int32(data->data.int_val);
            break;
        case MORLOC_FLOAT:
            token = mpack_pack_float(data->data.double_val);
            break;
        case MORLOC_STRING:
            token = mpack_pack_str(data->size);
            break;
        case MORLOC_BINARY:
            token = mpack_pack_bin(data->size);
            break;
        case MORLOC_BOOL_ARRAY:
        case MORLOC_INT_ARRAY:
        case MORLOC_FLOAT_ARRAY:
        case MORLOC_ARRAY:
        case MORLOC_TUPLE:
            // these all start with the same array bytes
            token = mpack_pack_array(data->size);
            break;
        case MORLOC_MAP:
            token = mpack_pack_map(data->size);
            break;
        default:
            fprintf(stderr, "Unexpected morloc type\n");
            return 1;
    }

    dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 0);

    size_t array_length;
    Schema* array_schema;

    switch(schema->type){
      case MORLOC_BINARY:
      case MORLOC_STRING:
        write_to_packet((void*)data->data.char_arr, packet, packet_ptr, packet_remaining, data->size);
        break;
      case MORLOC_ARRAY:
        array_length = data->size;
        array_schema = schema->parameters[0];
        for (size_t i = 0; i < array_length; i++) {
            pack_data(
              data->data.obj_arr[i],
              array_schema,
              packet,
              packet_ptr,
              packet_remaining,
              tokbuf
            );
        }
        break;
      case MORLOC_BOOL_ARRAY:
      case MORLOC_INT_ARRAY:
      case MORLOC_FLOAT_ARRAY:

        for (size_t i = 0; i < data->size; i++){

            switch(schema->type){
              case MORLOC_BOOL_ARRAY:
                token = mpack_pack_boolean(data->data.bool_arr[i]);
                break;
              case MORLOC_INT_ARRAY:
                token = mpack_pack_int32(data->data.int_arr[i]);
                break;
              case MORLOC_FLOAT_ARRAY:
                token = mpack_pack_float(data->data.float_arr[i]);
                break;
              default:
                fprintf(stderr, "Unexpected token: %d\n", schema->type);
                break;
            }

            // token.length isn't set for the primitives other that floats.
            // so I'm hard setting the required bases to 8, which is enough
            // for any supported number. In rare edge cases, this could lead
            // to the buffer being unnecessarily resized. But this will only
            // be a minor performance cost. mpack_pack_sint is about 10%
            // faster than mpack_pack_number, so usually sint is better.
            dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 8);

        }
        break;
      case MORLOC_TUPLE:
        for (size_t i = 0; i < schema->size; i++) {
            pack_data(
              data->data.obj_arr[i],
              schema->parameters[i],
              packet,
              packet_ptr,
              packet_remaining,
              tokbuf
            );
        }
        break;
      case MORLOC_MAP:
        for (size_t i = 0; i < data->size; i++) {

            char* key = data->data.obj_arr[i]->key;
            size_t key_len = strlen(key);

            // write key string token
            token = mpack_pack_str(key_len);
            dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 8);

            // write string bytes
            write_to_packet((void*)key, packet, packet_ptr, packet_remaining, key_len);

            // write value
            pack_data(data->data.obj_arr[i], schema->parameters[i], packet, packet_ptr, packet_remaining, tokbuf);
        }
        break;
    }

    return 0;
}



int pack_with_schema(const Anything* data, const Schema* schema, char** packet, size_t* packet_size) {
    *packet_size = 0;

    *packet = (char*)malloc(BUFFER_SIZE * sizeof(char));
    if (*packet == NULL) return 1;
    size_t packet_remaining = BUFFER_SIZE;
    char* packet_ptr = *packet;

    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);

    int pack_result = pack_data(data, schema, packet, &packet_ptr, &packet_remaining, &tokbuf);

    // mutate packet_size (will be used outside)
    *packet_size = packet_ptr - *packet;

    // Trim the output buffer to the exact size needed
    if (packet_remaining > 0) {
        *packet = (char*)realloc(*packet, *packet_size);
    }

    return pack_result;
}

int pack(const Anything* data, const char* schema_str, char** out_data, size_t* out_size) {
    Schema* schema = parse_schema(&schema_str);
    return pack_with_schema(data, schema, out_data, out_size);
}



void write_token(mpack_token_t token){
    switch(token.type){
        case MPACK_TOKEN_NIL:
            fprintf(stderr, "NIL");
            break;
        case MPACK_TOKEN_BOOLEAN:
            fprintf(stderr, "BOOLEAN");
            break;
        case MPACK_TOKEN_SINT:
            fprintf(stderr, "SINT(%d)", token.length);
            break;
        case MPACK_TOKEN_UINT:
            fprintf(stderr, "UINT(%d)", token.length);
            break;
        case MPACK_TOKEN_FLOAT:
            fprintf(stderr, "FLOAT");
            break;
        case MPACK_TOKEN_CHUNK:
            fprintf(stderr, "CHUNK(%d): ", token.length);
            if(token.data.chunk_ptr)
                print_hex(token.data.chunk_ptr, token.length);
            break;
        case MPACK_TOKEN_ARRAY:
            fprintf(stderr, "ARRAY(%d)", token.length);
            break;
        case MPACK_TOKEN_MAP:
            fprintf(stderr, "MAP(%d)", token.length);
            break;
        case MPACK_TOKEN_BIN:
            fprintf(stderr, "BIN(%d)", token.length);
            break;
        case MPACK_TOKEN_STR:
            fprintf(stderr, "STR(%d)", token.length);
            break;
        case MPACK_TOKEN_EXT:
            fprintf(stderr, "EXT(%d)", token.length);
            break;
      default:
        break;
    }
}

// terminal parsers
Anything* parse_binary(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_bool(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_bool_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_float(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_float_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_int(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_int_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_nil(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_string(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

// nested parsers
Anything* parse_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_map(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_obj(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

Anything* parse_nil(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return nil_data();
}

Anything* parse_bool(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return bool_data(mpack_unpack_boolean(*token));
}

Anything* parse_int(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    /* fprintf(stderr, "Is data being decoded incorrectly?\n"); */
    /* print_hex(*buf_ptr, *buf_remaining);            */
    /* fprintf(stderr, "\n");                                   */

    int result = -999;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    switch(token->type){
      case MPACK_TOKEN_UINT:
        result = (int)mpack_unpack_uint32(*token);
        break;
      case MPACK_TOKEN_SINT:
        result = (int)mpack_unpack_sint32(*token);
        break;
      case MPACK_TOKEN_FLOAT:
        result = (int)(mpack_unpack_float(*token));
        break;
      default:
        fprintf(stderr, "Bad token %d\n", token->type);
        break;
    }

    return int_data(result);
}

Anything* parse_float(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return float_data((double)mpack_unpack_float(*token));
}


char* parse_key(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    char* key = (char*)calloc(token->length + 1, sizeof(char));
    
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          key + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return key;
}

Anything* parse_string(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = string_data_(size);
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          result->data.char_arr + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return result;
}

Anything* parse_binary(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = binary_data_(size);
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          result->data.char_arr + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return result;
}

Anything* parse_bool_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_bool_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      result->data.bool_arr[i] = mpack_unpack_boolean(*token);
    }
    return result;
}

Anything* parse_int_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_int_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      switch(token->type){
        case MPACK_TOKEN_UINT:
          result->data.int_arr[i] = (int)mpack_unpack_uint(*token);
          break;
        case MPACK_TOKEN_SINT:
          result->data.int_arr[i] = (int)mpack_unpack_sint(*token);
          break;
        case MPACK_TOKEN_FLOAT:
          result->data.int_arr[i] = (int)(mpack_unpack_float(*token));
          break;
        default:
          fprintf(stderr, "Bad token %d\n", token->type);
          break;
      }
    }
    return result;
}

Anything* parse_float_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_float_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      result->data.float_arr[i] = (double)mpack_unpack_float(*token);
    }
    return result;
}

Anything* parse_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_data_(size);

    for(size_t i = 0; i < size; i++){
        result->data.obj_arr[i] = parse_obj(schema, tokbuf, buf_ptr, buf_remaining, token);
    }

    return result;
}

Anything* parse_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = tuple_data_(size);

    for(size_t i = 0; i < size; i++){
        result->data.obj_arr[i] = parse_obj(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
    }

    return result;
}

Anything* parse_map(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = map_data_(size);
    char* key;

    for(size_t i = 0; i < size; i++){
        key = parse_key(tokbuf, buf_ptr, buf_remaining, token);;
        result->data.obj_arr[i] = parse_obj(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
        result->data.obj_arr[i]->key = key;
    }

    return result;
}

Anything* parse_obj(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    switch(schema->type){
      case MORLOC_NIL:
        return parse_nil(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL:
        return parse_bool(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_INT:
        return parse_int(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_FLOAT:
        return parse_float(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_STRING:
        return parse_string(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BINARY:
        return parse_binary(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_ARRAY:
        return parse_array(schema->parameters[0], tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_MAP:
        return parse_map(schema, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_TUPLE:
        return parse_tuple(schema, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL_ARRAY:
        return parse_bool_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_INT_ARRAY:
        return parse_int_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_FLOAT_ARRAY:
        return parse_float_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_EXT:
        fprintf(stderr, "Unexpected schema type in parse_obj\n");
        break;
      default:
        break;
    }
    return NULL;
}

void write_tokens(const char** buf_ptr, size_t* buf_remaining){
    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);
    mpack_token_t token;

    int read_result;

    do {
      read_result = mpack_read(&tokbuf, buf_ptr, buf_remaining, &token);
      write_token(token);
      fprintf(stderr, "\n");
    } while(read_result == MPACK_OK);
}


int unpack_with_schema(const char* data, size_t data_size, const Schema* schema, Anything** out_data) {
    // Use the existing unpack_with_schema function, but adapt it to the new prototype
    const char* buf = data;
    size_t buf_remaining = data_size;

    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);
    mpack_token_t token;

    *out_data = parse_obj(schema, &tokbuf, &data, &buf_remaining, &token);

    // Return 0 for success, or an error code if unpack_with_schema fails
    return (*out_data != NULL) ? 0 : -1;
}

int unpack(const char* data, size_t data_size, const char* schema_str, Anything** out_data) {
    const Schema* schema = parse_schema(&schema_str);
    return unpack_with_schema(data, data_size, schema, out_data);
}



// Function to get the string representation of morloc_serial_type
const char* get_type_string(morloc_serial_type type) {
    switch (type) {
        case MORLOC_NIL: return "NIL";
        case MORLOC_BOOL: return "BOOL";
        case MORLOC_INT: return "INT";
        case MORLOC_FLOAT: return "FLOAT";
        case MORLOC_STRING: return "STRING";
        case MORLOC_BINARY: return "BINARY";
        case MORLOC_ARRAY: return "ARRAY";
        case MORLOC_MAP: return "MAP";
        case MORLOC_TUPLE: return "TUPLE";
        case MORLOC_BOOL_ARRAY: return "BOOL_ARRAY";
        case MORLOC_INT_ARRAY: return "INT_ARRAY";
        case MORLOC_FLOAT_ARRAY: return "FLOAT_ARRAY";
        case MORLOC_EXT: return "EXT";
        default: return "UNKNOWN";
    }
}

// Function to print Schema
void print_schema_r(const Schema* schema, int indent) {
    if (schema == NULL) {
        fprintf(stderr, "null");
        return;
    }

    fprintf(stderr, "{\n");
    fprintf(stderr, "%*s\"type\": \"%s\",\n", indent + 2, "", get_type_string(schema->type));
    fprintf(stderr, "%*s\"size\": %zu", indent + 2, "", schema->size);

    if (schema->size > 0) {
        fprintf(stderr, ",\n%*s\"parameters\": [\n", indent + 2, "");
        for (size_t i = 0; i < schema->size; i++) {
            fprintf(stderr, "%*s", indent + 4, "");
            print_schema_r(schema->parameters[i], indent + 4);
            if (i < schema->size - 1) fprintf(stderr, ",");
            fprintf(stderr, "\n");
        }
        fprintf(stderr, "%*s]", indent + 2, "");

        if (schema->keys != NULL) {
            fprintf(stderr, ",\n%*s\"keys\": [", indent + 2, "");
            for (size_t i = 0; i < schema->size; i++) {
                fprintf(stderr, "\"%s\"", schema->keys[i]);
                if (i < schema->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
        }
    }

    fprintf(stderr, "\n%*s}", indent, "");
}

void print_schema(const Schema* schema){
  print_schema_r(schema, 0);
}



// Function to print Anything
void print_parsed_data_r(const Anything* data, int indent) {
    if (data == NULL) {
        fprintf(stderr, "null");
        return;
    }

    fprintf(stderr, "{\n");
    fprintf(stderr, "%*s\"type\": \"%s\",\n", indent + 2, "", get_type_string(data->type));
    fprintf(stderr, "%*s\"size\": %zu,\n", indent + 2, "", data->size);
    
    if (data->key != NULL) {
        fprintf(stderr, "%*s\"key\": \"%s\",\n", indent + 2, "", data->key);
    }

    fprintf(stderr, "%*s\"data\": ", indent + 2, "");

    switch (data->type) {
        case MORLOC_NIL:
            fprintf(stderr, "null");
            break;
        case MORLOC_BOOL:
            fprintf(stderr, "%s", data->data.bool_val ? "true" : "false");
            break;
        case MORLOC_INT:
            fprintf(stderr, "%d", data->data.int_val);
            break;
        case MORLOC_FLOAT:
            fprintf(stderr, "%f", data->data.double_val);
            break;
        case MORLOC_STRING:
            fprintf(stderr, "\"%s\"", data->data.char_arr);
            break;
        case MORLOC_BINARY:
            fprintf(stderr, "\"<binary data>\"");
            break;
        case MORLOC_ARRAY:
        case MORLOC_MAP:
        case MORLOC_TUPLE:
            fprintf(stderr, "[\n");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%*s", indent + 4, "");
                print_parsed_data_r(data->data.obj_arr[i], indent + 4);
                if (i < data->size - 1) fprintf(stderr, ",");
                fprintf(stderr, "\n");
            }
            fprintf(stderr, "%*s]", indent + 2, "");
            break;
        case MORLOC_BOOL_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%s", data->data.bool_arr[i] ? "true" : "false");
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_INT_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%d", data->data.int_arr[i]);
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_FLOAT_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%f", data->data.float_arr[i]);
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_EXT:
            fprintf(stderr, "\"<extension data>\"");
            break;
    }

    fprintf(stderr, "\n%*s}", indent, "");
}

void print_parsed_data(const Anything* data){
  print_parsed_data_r(data, 0);
}

#endif // ending __MLCMPACK_HEADER_ONLY_H__
