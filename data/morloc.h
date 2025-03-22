#ifndef __MORLOC_H__
#define __MORLOC_H__

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <limits.h>
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

#define FREE(ptr) if(ptr != NULL){ free(ptr); ptr = NULL; }

// {{{ Error handling macro definictions

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

/// }}}

// {{{ utilities

// just a debugging function
void hex(const void *ptr, size_t size) {
    unsigned char *byte_ptr = (unsigned char *)ptr;
    for (size_t i = 0; i < size; i++) {
        if(i > 0 && i % 8 == 0){
          fprintf(stderr, " ");
        }
        fprintf(stderr, "%02X", byte_ptr[i]);
        if (i < size - 1) {
            fprintf(stderr, " ");
        }
    }
}

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

MPACK_API int mpack_read(mpack_tokbuf_t *tb, const char **b, size_t *bl,
    mpack_token_t *tok) FUNUSED FNONULL;
MPACK_API int mpack_write(mpack_tokbuf_t *tb, char **b, size_t *bl,
    const mpack_token_t *tok) FUNUSED FNONULL;

#ifndef bool
# define bool unsigned
#endif

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

#define UNUSED(p) (void)p;
#define ADVANCE(buf, buflen) ((*buflen)--, (unsigned char)*((*buf)++))
#define TLEN(val, range_start) ((uint32_t)(1 << (val - range_start)))
#ifndef MIN
# define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

static int mpack_rtoken(const char **buf, size_t *buflen,
    mpack_token_t *tok);
static int mpack_rpending(const char **b, size_t *nl, mpack_tokbuf_t *tb);
static int mpack_rvalue(mpack_token_type_t t, uint32_t l,
    const char **b, size_t *bl, mpack_token_t *tok);
static int mpack_rblob(mpack_token_type_t t, uint32_t l,
    const char **b, size_t *bl, mpack_token_t *tok);
static int mpack_wtoken(const mpack_token_t *tok, char **b, size_t *bl);
static int mpack_wpending(char **b, size_t *bl, mpack_tokbuf_t *tb);
static int mpack_wpint(char **b, size_t *bl, mpack_value_t v);
static int mpack_wnint(char **b, size_t *bl, mpack_value_t v);
static int mpack_wfloat(char **b, size_t *bl, const mpack_token_t *v);
static int mpack_wstr(char **buf, size_t *buflen, uint32_t len);
static int mpack_wbin(char **buf, size_t *buflen, uint32_t len);
static int mpack_wext(char **buf, size_t *buflen, int type,
    uint32_t len);
static int mpack_warray(char **buf, size_t *buflen, uint32_t len);
static int mpack_wmap(char **buf, size_t *buflen, uint32_t len);
static int mpack_w1(char **b, size_t *bl, uint32_t v);
static int mpack_w2(char **b, size_t *bl, uint32_t v);
static int mpack_w4(char **b, size_t *bl, uint32_t v);
static mpack_value_t mpack_byte(unsigned char b);
static int mpack_value(mpack_token_type_t t, uint32_t l,
    mpack_value_t v, mpack_token_t *tok);
static int mpack_blob(mpack_token_type_t t, uint32_t l, int et,
    mpack_token_t *tok);

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
    tok->length = MIN((uint32_t)*buflen, tokbuf->passthrough);
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

static int mpack_rvalue(mpack_token_type_t type, uint32_t remaining,
    const char **buf, size_t *buflen, mpack_token_t *tok)
{
  if (*buflen < remaining) {
    tok->length = remaining;
    return MPACK_EOF;
  }

  mpack_value(type, remaining, mpack_byte(0), tok);

  while (remaining) {
    uint32_t byte = ADVANCE(buf, buflen), byte_idx, byte_shift;
    byte_idx = (uint32_t)--remaining;
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
    uint32_t hi = tok->data.value.hi;
    uint32_t lo = tok->data.value.lo;
    uint32_t msb = (tok->length == 8 && hi >> 31) ||
                         (tok->length == 4 && lo >> 31) ||
                         (tok->length == 2 && lo >> 15) ||
                         (tok->length == 1 && lo >> 7);
    if (!msb) {
      tok->type = MPACK_TOKEN_UINT;
    }
  }

  return MPACK_OK;
}

static int mpack_rblob(mpack_token_type_t type, uint32_t tlen,
    const char **buf, size_t *buflen, mpack_token_t *tok)
{
  mpack_token_t l;
  uint32_t required = tlen + (type == MPACK_TOKEN_EXT ? 1 : 0);

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
  uint32_t hi = val.hi;
  uint32_t lo = val.lo;

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
  uint32_t hi = val.hi;
  uint32_t lo = val.lo;

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
    return mpack_w1(buf, buflen, (uint32_t)(0x100 + lo));
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

static int mpack_wstr(char **buf, size_t *buflen, uint32_t len)
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

static int mpack_wbin(char **buf, size_t *buflen, uint32_t len)
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
    uint32_t len)
{
  uint32_t t;
  assert(type >= 0 && type < 0x80);
  t = (uint32_t)type;
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

static int mpack_warray(char **buf, size_t *buflen, uint32_t len)
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

static int mpack_wmap(char **buf, size_t *buflen, uint32_t len)
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

static int mpack_w1(char **b, size_t *bl, uint32_t v)
{
  (*bl)--;
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_w2(char **b, size_t *bl, uint32_t v)
{
  *bl -= 2;
  *(*b)++ = (char)((v >> 8) & 0xff);
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_w4(char **b, size_t *bl, uint32_t v)
{
  *bl -= 4;
  *(*b)++ = (char)((v >> 24) & 0xff);
  *(*b)++ = (char)((v >> 16) & 0xff);
  *(*b)++ = (char)((v >> 8) & 0xff);
  *(*b)++ = (char)(v & 0xff);
  return MPACK_OK;
}

static int mpack_value(mpack_token_type_t type, uint32_t length,
    mpack_value_t value, mpack_token_t *tok)
{
  tok->type = type;
  tok->length = length;
  tok->data.value = value;
  return MPACK_OK;
}

static int mpack_blob(mpack_token_type_t type, uint32_t length,
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
static int mpack_is_be(void) FPURE;


#define POW2(n) \
  ((double)(1 << (n / 2)) * (double)(1 << (n / 2)) * (double)(1 << (n % 2)))

#define MPACK_SWAP_VALUE(val)                                  \
  do {                                                         \
    uint32_t lo = val.lo;                                      \
    val.lo = val.hi;                                           \
    val.hi = lo;                                               \
  } while (0)

MPACK_API mpack_token_t mpack_pack_nil(void)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_NIL;
  rv.length = 1;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_boolean(unsigned v)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_BOOLEAN;
  rv.data.value.lo = v ? 1 : 0;
  rv.data.value.hi = 0;
  rv.length = 1;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_uint(uint64_t v)
{
  mpack_token_t rv;
  rv.data.value.lo = v & 0xffffffff;
  rv.data.value.hi = (uint32_t)((v >> 31) >> 1);
  rv.type = MPACK_TOKEN_UINT;
  rv.length = 8;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_sint(int64_t v)
{
  if (v < 0) {
    mpack_token_t rv;
    uint64_t tc = -((uint64_t)(v + 1)) + 1;
    tc = ~tc + 1;
    rv = mpack_pack_uint(tc);
    rv.type = MPACK_TOKEN_SINT;
  rv.length = 8;
    return rv;
  }

  return mpack_pack_uint((uint64_t)v);
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
  rv.length = 4;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_float(double v)
{
  /* ieee754 single-precision limits to determine if "v" can be fully
   * represented in 4 bytes */
  mpack_token_t rv;

  if (mpack_fits_single(v)) {
    union {
      float f;
      uint32_t m;
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

MPACK_API mpack_token_t mpack_pack_str(uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_STR;
  rv.length = l;
  return rv;
}

MPACK_API mpack_token_t mpack_pack_array(uint32_t l)
{
  mpack_token_t rv;
  rv.type = MPACK_TOKEN_ARRAY;
  rv.length = l;
  return rv;
}

MPACK_API bool mpack_unpack_boolean(mpack_token_t t)
{
  return t.data.value.lo || t.data.value.hi;
}

MPACK_API uint64_t mpack_unpack_uint(mpack_token_t t)
{
  return (((uint64_t)t.data.value.hi << 31) << 1) | t.data.value.lo;
}

/* unpack signed integer without relying on two's complement as internal
 * representation */
MPACK_API int64_t mpack_unpack_sint(mpack_token_t t)
{
  uint32_t hi = t.data.value.hi;
  uint32_t lo = t.data.value.lo;
  uint64_t rv = lo;
  // This assert fails because length is not initialized
  // The problem has probably gone undetected because asserts disappear with -O
  /* assert(t.length <= sizeof(int64_t)); */

  rv |= (((uint64_t)hi) << 31) << 1;

  /* reverse the two's complement so that lo/hi contain the absolute value.
   * note that we have to mask ~rv so that it reflects the two's complement
   * of the appropriate byte length */
  rv = (~rv & (((uint64_t)1 << ((t.length * 8) - 1)) - 1)) + 1;
  /* negate and return the absolute value, making sure int64_t can
   * represent the positive cast. */
  return -((int64_t)(rv - 1)) - 1;
}

MPACK_API double mpack_unpack_float(mpack_token_t t)
{
  if (t.length == 4) {
    union {
      float f;
      uint32_t m;
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

static int mpack_fits_single(double v)
{
  return (float)v == v;
}

static int mpack_is_be(void)
{
  union {
    uint32_t i;
    char c[sizeof(uint32_t)];
  } test;

  test.i = 1;
  return test.c[0] == 0;
}

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

typedef struct block_header_s {
    // a constant magic number identifying a block header
    unsigned int magic;
    // the number of references to this block
    unsigned int reference_count;
    // the amount of memory that is stored in the header
    size_t size;
} block_header_t;


// The index of the current volum
static size_t current_volume = 0;
static char common_basename[MAX_FILENAME_SIZE];

static shm_t* volumes[MAX_VOLUME_NUMBER] = {NULL};

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG);
shm_t* shopen(size_t volume_index, ERRMSG);
bool shclose(ERRMSG);
void* shmalloc(size_t size, ERRMSG);
void* shmemcpy(void* dest, size_t size, ERRMSG);
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

volptr_t rel2vol(relptr_t ptr, ERRMSG) {
    VAL_RETURN_SETUP(volptr_t, VOLNULL)

    for (size_t i = 0; i < MAX_VOLUME_NUMBER; i++) {
        shm_t* shm = shopen(i, &CHILD_ERRMSG);
        if (shm == NULL) {
            RAISE_IF(CHILD_ERRMSG == NULL, "Shared volume %zu does not exist", ptr)
            RAISE("Error occured while opening shared volume %zu:\n%s", ptr, CHILD_ERRMSG)
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
        shm_t* shm = shopen(i, &CHILD_ERRMSG);
        if (shm == NULL) {
            RAISE_IF(CHILD_ERRMSG == NULL, "Failed to find shared volume %zu while searching for relative pointer %zu", i, ptr)
            RAISE("Error occured while opening shared volume %zu while searching for %zu:\n%s", i, ptr, CHILD_ERRMSG)
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

// No errors are raised here, pointers that map outside the volume return -1
//
// This function is a bit odd ... maybe we should kill it?
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



// Get and check a block header given a pointer to the beginning of a block's
// data section
block_header_t* abs2blk(void* ptr, ERRMSG){
    PTR_RETURN_SETUP(block_header_t)

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));

    RAISE_IF(blk && blk->magic != BLK_MAGIC, "Bad block magic")

    return blk;
}

shm_t* shinit(const char* shm_basename, size_t volume_index, size_t shm_size, ERRMSG) {
    PTR_RETURN_SETUP(shm_t)

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
        close(fd),
        "Failed to fstat '%s'",
        shm_name
    )

    // Check if we've just created the shared memory object
    bool created = (sb.st_size == 0);
    RAISE_IF_WITH(
        created && ftruncate(fd, full_size) == -1,
        close(fd),
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
        
        // Initialize the read-write lock
        if (pthread_rwlock_init(&shm->rwlock, NULL) != 0){
            munmap(volumes[volume_index], full_size);
            close(fd);
            RAISE("Failed initialize read-write lock on '%s'", shm_name)
        }

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



// like shinit, but only opens existing share memory volumes if the exist
// if no volume exists, return NULL
// Non-existence of the volume is not considered an error
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


bool shclose(ERRMSG) {
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
    bool found_memory = get_available_memory(&available_memory, &CHILD_ERRMSG);

    RAISE_IF(!found_memory, "Failed to find available system memory:\n%s", CHILD_ERRMSG)

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
            
            if ((char*)next_blk >= end || next_blk->reference_count != 0) {
                break;
            }

            // merge the blocks
            blk->size += sizeof(block_header_t) + next_blk->size;
            // set the merged block to 0, this step could be skipped
            memset(next_blk, 0, sizeof(block_header_t) + next_blk->size);
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

    // try to get the current block at the cursor
    block_header_t* blk = get_block(shm, shm->cursor, &CHILD_ERRMSG);
    FREE(CHILD_ERRMSG)

    if (blk != NULL && blk->size >= size + sizeof(block_header_t) && blk->reference_count == 0) {
        return blk;
    }

    char* shm_end = (char*)shm + sizeof(shm_t) + shm->volume_size;

    // Lock this pool while searching for a non-cursor block. This is necessary
    // since adjacent free blocks will be merged, which could potentially lead to
    // conflicts.
    if (pthread_rwlock_wrlock(&shm->rwlock) != 0) {
        RAISE("Failed to acquire write lock")
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

// Find a free block that can allocate a given size of memory. If no lbock is
// found, create a new volume.
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
        bool volume_created = choose_next_volume_size(&new_volume_size, size, &CHILD_ERRMSG);
        RAISE_IF(!volume_created, "\n%s", CHILD_ERRMSG)

        shm = shinit(common_basename, i, new_volume_size, &CHILD_ERRMSG);
        RAISE_IF(shm == NULL, "\n%s", CHILD_ERRMSG)

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
    RAISE_IF(new_cursor == VOLNULL, "\n%s", CHILD_ERRMSG)

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


void* shmalloc(size_t size, ERRMSG) {
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
            final_blk->reference_count++;
            return (void*)(final_blk + 1);
        }
    }

    RAISE("Failed to allocate shared memory block of size %zu:\n%s", size, CHILD_ERRMSG);
}

void* shmemcpy(void* dest, size_t size, ERRMSG){
    PTR_RETURN_SETUP(void)

    void* src = shmalloc(size, &CHILD_ERRMSG);

    RAISE_IF(src == NULL, "Failed to copy:\n%s", CHILD_ERRMSG)

    memmove(dest, src, size);

    return src;
}

void* shcalloc(size_t nmemb, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    void* data = shmalloc(nmemb * size, &CHILD_ERRMSG);

    RAISE_IF(data == NULL, "Failed to copy:\n%s", CHILD_ERRMSG)

    memset(data, 0, nmemb * size);

    return data;
}


void* shrealloc(void* ptr, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(size == 0, "Cannot reallocate to size 0")

    if (ptr == NULL){
        ptr = shmalloc(size, &CHILD_ERRMSG);
        RAISE_IF(ptr == NULL, "Failed to allocate new block\n%s", CHILD_ERRMSG)
    }

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));
    shm_t* shm = abs2shm(ptr, &CHILD_ERRMSG);
    RAISE_IF(shm == NULL, "Failed to open shared memory drive\n%s", CHILD_ERRMSG)

    void* new_ptr;

    if (blk->size >= size) {
        // The current block is large enough
        new_ptr = split_block(shm, blk, size, &CHILD_ERRMSG);
        RAISE_IF(new_ptr == NULL, "Failed to split block\n%s", CHILD_ERRMSG)
    } else {
        // Need to allocate a new block
        new_ptr = shmalloc(size, &CHILD_ERRMSG);
        RAISE_IF(new_ptr == NULL, "Failed to allocate new block\n%s", CHILD_ERRMSG)

        pthread_rwlock_wrlock(&shm->rwlock);
        memcpy(new_ptr, ptr, blk->size);
        pthread_rwlock_unlock(&shm->rwlock);

        bool freed = shfree(ptr, &CHILD_ERRMSG);
        RAISE_IF(!freed, "\n%s", CHILD_ERRMSG)

        return new_ptr;
    }

    return new_ptr;
}


// Free a chunk of memory. The pointer points to the start of the memory that
// the user is given relative to the user's process. The block header is just
// upstream of this position.
//
// return true for success
bool shfree(absptr_t ptr, ERRMSG) {
    BOOL_RETURN_SETUP
    RAISE_IF(ptr == NULL, "Invalid or inaccessible shared memory pool pointer - perhaps the pool is closed?");

    block_header_t* blk = (block_header_t*)((char*)ptr - sizeof(block_header_t));

    RAISE_IF(blk == NULL, "Out-of-bounds relative pointer");
    RAISE_IF(blk->magic != BLK_MAGIC, "Corrupted memory");
    RAISE_IF(blk->reference_count == 0, "Cannot free memory, reference count is already 0"); 

    // This is an atomic operation, so no need to lock
    blk->reference_count--;

    if (blk->reference_count == 0) {
        // Set memory to 0, this may be perhaps be removed in the future for
        // performance sake, but for now it helps with diagnostics. Note that the
        // head remains, it may be used to merge free blocks in the future.
        memset(blk + 1, 0, blk->size);
    }

    return true;
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

#define BUFFER_SIZE 4096

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

// Allocate a shared memory block sufficient to store a schema's object
void* get_ptr(const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(void)
    void* ptr = (void*)shmalloc(schema->width, &CHILD_ERRMSG);
    RAISE_IF(ptr == NULL, "Allocation failed:\n%s", CHILD_ERRMSG) 
    return ptr;
}

// The voidstar representation of variable length data
typedef struct Array {
  size_t size;
  relptr_t data;
} Array;

// Prototypes

Schema* parse_schema(const char** schema_ptr, ERRMSG);

// Helper function to create a schema with parameters
static Schema* create_schema_with_params(morloc_serial_type type, size_t width, size_t size, Schema** params, char** keys) {
    Schema* schema = (Schema*)malloc(sizeof(Schema));
    if (!schema) return NULL;

    schema->type = type;
    schema->size = size;
    schema->width = width;
    if(size > 0){
        schema->offsets = (size_t*)calloc(size, sizeof(size_t));;
    } else {
        schema->offsets = NULL;
    }
    schema->parameters = params;
    schema->keys = keys;

    // for tuples and maps, generate the element offsets
    if(params){
      for(size_t i = 1; i < size; i++){
        schema->offsets[i] = schema->offsets[i-1] + params[i-1]->width;
      }
    }

    return schema;
}

static Schema* nil_schema() {
    return create_schema_with_params(MORLOC_NIL, 1, 0, NULL, NULL);
}

static Schema* bool_schema() {
    return create_schema_with_params(MORLOC_BOOL, 1, 0, NULL, NULL);
}

static Schema* uint_schema(size_t width) {
    switch(width){
      case 1:
        return create_schema_with_params(MORLOC_UINT8, width, 0, NULL, NULL);
      case 2:
        return create_schema_with_params(MORLOC_UINT16, width, 0, NULL, NULL);
      case 4:
        return create_schema_with_params(MORLOC_UINT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_UINT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Integers may only have widths of 1, 2, 4, or 8 bytes; found %lu", width);
        return NULL;
    }
}

static Schema* sint_schema(size_t width) {
    switch(width){
      case 1:
        return create_schema_with_params(MORLOC_SINT8, width, 0, NULL, NULL);
      case 2:
        return create_schema_with_params(MORLOC_SINT16, width, 0, NULL, NULL);
      case 4:
        return create_schema_with_params(MORLOC_SINT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_SINT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Integers may only have widths of 1, 2, 4, or 8 bytes; found %lu", width);
        return NULL;
    }
}

static Schema* float_schema(size_t width) {
    switch(width){
      case 4:
        return create_schema_with_params(MORLOC_FLOAT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_FLOAT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Floats may only have widths of 4 or 8 bytes, found %lu\n", width);
        return NULL;
    }
}

static Schema* string_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    if (!params) return NULL;

    // This parameter is needed for compatibility with arrays
    params[0] = uint_schema(1);

    return create_schema_with_params(MORLOC_STRING, sizeof(Array), 0, params, NULL);
}

static Schema* tuple_schema(Schema** params, size_t size) {
    size_t width = 0;
    for(size_t i = 0; i < size; i++){
      width += params[i]->width;
    }
    return create_schema_with_params(MORLOC_TUPLE, width, size, params, NULL);
}


static Schema* array_schema(Schema* array_type) {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    if (!params) return NULL;

    params[0] = array_type;

    return create_schema_with_params(MORLOC_ARRAY, sizeof(Array), 1, params, NULL);
}

static Schema* map_schema(size_t size, char** keys, Schema** params) {
    size_t width = 0;
    for(size_t i = 0; i < size; i++){
      width += params[i]->width;
    }
    return create_schema_with_params(MORLOC_MAP, width, size, params, keys);
}

static size_t parse_schema_size(const char** schema_ptr){
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

static char* parse_schema_key(const char** schema_ptr){
  size_t key_size = parse_schema_size(schema_ptr);
  char* key = (char*)calloc(key_size+1, sizeof(char));
  memcpy(key, *schema_ptr, key_size);
  *schema_ptr += key_size;
  return key;
}

// This parser starts on the character **after** the initial '<'
static char* parse_hint(const char** schema_ptr) {
  char* hint = NULL;

  if (!schema_ptr || !*schema_ptr) {
    return hint;
  }

  size_t depth = 1;
  size_t buffer_size = 128;
  size_t buffer_index = 0;
  hint = (char*)malloc(buffer_size);
  if (!hint) return NULL;

  while (**schema_ptr != '\0') {
    if (**schema_ptr == '<') {
      depth++;
    } else if (**schema_ptr == '>') {
      depth--;
      if (depth == 0) {
        (*schema_ptr)++; // Move past the closing '>'
        break;
      }
    }

    if (buffer_index >= buffer_size - 1) {
      buffer_size *= 2;
      char* new_hint = (char*)realloc(hint, buffer_size);
      if (!new_hint) {
        FREE(hint);
        return NULL;
      }
      hint = new_hint;
    }

    hint[buffer_index++] = **schema_ptr;
    (*schema_ptr)++;
  }

  if (depth != 0) {
    // Unmatched '<', free memory and return NULL
    FREE(hint);
    return NULL;
  }

  hint[buffer_index] = '\0';
  return hint;
}

Schema* parse_schema(const char** schema_ptr, ERRMSG){
  PTR_RETURN_SETUP(Schema)

  Schema** params;
  char c = **schema_ptr;
  (*schema_ptr)++;
  size_t size;
  char** keys; 
  Schema* child_schema = NULL;

  Schema* schema = NULL;
  char* hint = NULL;

  switch(c){
    case SCHEMA_ARRAY:
      child_schema = parse_schema(schema_ptr, &CHILD_ERRMSG);
      RAISE_IF(child_schema == NULL, "\n%s", CHILD_ERRMSG)
      schema = array_schema(child_schema);
      break;
    case SCHEMA_TUPLE:
      size = parse_schema_size(schema_ptr);
      params = (Schema**)calloc(size, sizeof(Schema*));
      for(size_t i = 0; i < size; i++){
        params[i] = parse_schema(schema_ptr, &CHILD_ERRMSG);
        RAISE_IF(params[i] == NULL, "\n%s", CHILD_ERRMSG)
      }
      schema = tuple_schema(params, size);
      break;
    case SCHEMA_MAP:
      size = parse_schema_size(schema_ptr);
      keys = (char**)calloc(size, sizeof(char*));
      params = (Schema**)calloc(size, sizeof(Schema*));
      for(size_t i = 0; i < size; i++){
        keys[i] = parse_schema_key(schema_ptr);
        params[i] = parse_schema(schema_ptr, &CHILD_ERRMSG);
        RAISE_IF(params[i] == NULL, "\n%s", CHILD_ERRMSG)
      }
      schema = map_schema(size, keys, params);
      break;
    case SCHEMA_NIL:
      schema = nil_schema();
      break;
    case SCHEMA_BOOL:
      schema = bool_schema();
      break;
    case SCHEMA_SINT:
      size = parse_schema_size(schema_ptr);
      schema = sint_schema(size);
      break;
    case SCHEMA_UINT:
      size = parse_schema_size(schema_ptr);
      schema = uint_schema(size);
      break;
    case SCHEMA_FLOAT:
      size = parse_schema_size(schema_ptr);
      schema = float_schema(size);
      break;
    case SCHEMA_STRING:
      schema = string_schema();
      break;
    case '<':
      {
          hint = parse_hint(schema_ptr);
          schema = parse_schema(schema_ptr, &CHILD_ERRMSG);
          RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)
      }
      break;
    default:
      RAISE("Unrecognized schema type '%c'\n", c);
  }

  schema->hint = hint;

  return schema;
}


void free_schema(Schema* schema) {
    if (schema == NULL) {
        return;
    }

    FREE(schema->hint);
    FREE(schema->offsets);

    // Free the parameters and their contents
    if (schema->parameters != NULL) {
        for (size_t i = 0; i < schema->size; i++) {
            free_schema(schema->parameters[i]);
        }
        free(schema->parameters);
    }

    // Free the keys and their contents
    if (schema->keys != NULL) {
        for (size_t i = 0; i < schema->size; i++) {
            FREE(schema->keys[i]);
        }
        free(schema->keys);
    }

    // Finally, free the schema itself
    FREE(schema);
}

// }}} end morloc schema

// {{{ morloc MessagePack support

// Main pack function for creating morloc-encoded MessagePack data
int pack(const void* mlc, const char* schema_str, char** mpkptr, size_t* mpk_size, ERRMSG);
int pack_with_schema(const void* mlc, const Schema* schema, char** mpkptr, size_t* mpk_size, ERRMSG);

int unpack(const char* mpk, size_t mpk_size, const char* schema_str, void** mlcptr, ERRMSG);
int unpack_with_schema(const char* mpk, size_t mpk_size, const Schema* schema, void** mlcptr, ERRMSG);

// Try to add `added_size` bytes of space to a buffer, if there is not enough
// space, increase the buffer size.
static void upsize(
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


// write data to a packet, if the buffer is too small, increase its size
static void write_to_packet(
  const void* src,                // source data
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


// write a token to a packet, increase buffer size as needed
static int dynamic_mpack_write(
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


//  The main function for writing MessagePack
static EXIT_CODE pack_data(
  const void* mlc,           // input data structure
  const Schema* schema,      // input data schema
  char** packet,             // a pointer to the messagepack data
  char** packet_ptr,         // the current position in the buffer
  size_t* packet_remaining,  // bytes from current position to the packet end
  mpack_tokbuf_t* tokbuf,
  ERRMSG
) {
    INT_RETURN_SETUP

    mpack_token_t token;
    Array* array;

    switch (schema->type) {
        case MORLOC_NIL:
            token = mpack_pack_nil();
            break;
        case MORLOC_BOOL:
            token = mpack_pack_boolean(*(uint8_t*)mlc != 0);
            break;
        case MORLOC_UINT8:
            token = mpack_pack_uint((uint64_t)*(uint8_t*)mlc);
            break;
        case MORLOC_UINT16:
            token = mpack_pack_uint((uint64_t)*(uint16_t*)mlc);
            break;
        case MORLOC_UINT32:
            token = mpack_pack_uint((uint64_t)*(uint32_t*)mlc);
            break;
        case MORLOC_UINT64:
            token = mpack_pack_uint(*(uint64_t*)mlc);
            break;
        case MORLOC_SINT8:
            token = mpack_pack_sint((int64_t)*(int8_t*)mlc);
            break;
        case MORLOC_SINT16:
            token = mpack_pack_sint((int64_t)*(int16_t*)mlc);
            break;
        case MORLOC_SINT32:
            token = mpack_pack_sint((int64_t)*(int32_t*)mlc);
            break;
        case MORLOC_SINT64:
            token = mpack_pack_sint(*(int64_t*)mlc);
            break;
        case MORLOC_FLOAT32:
            token = mpack_pack_float((double)*(float*)mlc);
            break;
        case MORLOC_FLOAT64:
            token = mpack_pack_float(*(double*)mlc);
            break;
        case MORLOC_STRING:
            token = mpack_pack_str(((Array*)mlc)->size);
            break;
        case MORLOC_ARRAY:
            array = (Array*)mlc;
            token = mpack_pack_array(array->size);
            break;
        case MORLOC_MAP:
        case MORLOC_TUPLE:
            token = mpack_pack_array(schema->size);
            break;
        default:
            RAISE("Unexpected morloc type")
    }

    dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 0);

    size_t array_length;
    size_t array_width;
    void* voidstar_data = NULL;
    Schema* array_schema;

    switch(schema->type){
      case MORLOC_STRING:
        array = (Array*)mlc;

        voidstar_data = rel2abs(array->data, &CHILD_ERRMSG);
        RAISE_IF(voidstar_data == NULL, "\n%s", CHILD_ERRMSG)

        write_to_packet(voidstar_data, packet, packet_ptr, packet_remaining, array->size);
        break;
      case MORLOC_ARRAY:
        {
          array_length = ((Array*)mlc)->size;

          char* data = (char*)rel2abs(((Array*)mlc)->data, &CHILD_ERRMSG);
          RAISE_IF(data == NULL, "\n%s", CHILD_ERRMSG)

          array_schema = schema->parameters[0];
          array_width = array_schema->width;

          for (size_t i = 0; i < array_length; i++) {
              int exit_code = pack_data(
                  data + i * array_width,
                  array_schema,
                  packet,
                  packet_ptr,
                  packet_remaining,
                  tokbuf,
                  &CHILD_ERRMSG
              );
              RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
          }

        }
        break;
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        {
            for (size_t i = 0; i < schema->size; i++) {
                int exit_code = pack_data(
                    (char*)mlc + schema->offsets[i],
                    schema->parameters[i],
                    packet,
                    packet_ptr,
                    packet_remaining,
                    tokbuf,
                    &CHILD_ERRMSG
                );
                RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
            }
        }
        break;
      case MORLOC_NIL:
      case MORLOC_BOOL:
      case MORLOC_SINT8:
      case MORLOC_SINT16:
      case MORLOC_SINT32:
      case MORLOC_SINT64:
      case MORLOC_UINT8:
      case MORLOC_UINT16:
      case MORLOC_UINT32:
      case MORLOC_UINT64:
      case MORLOC_FLOAT32:
      case MORLOC_FLOAT64:
        // no further processing needed
        break;
    }

    return EXIT_PASS;
}


#define MPACK_TOKBUF_INITIAL_VALUE { { 0 }, { (mpack_token_type_t)0, 0, { .value = { 0 } } }, 0, 0, 0 }

int pack_with_schema(const void* mlc, const Schema* schema, char** packet, size_t* packet_size, ERRMSG) {
    INT_RETURN_SETUP

    *packet_size = 0;

    *packet = (char*)malloc(BUFFER_SIZE * sizeof(char));
    if (*packet == NULL) return 1;
    size_t packet_remaining = BUFFER_SIZE;
    char* packet_ptr = *packet;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;

    int pack_result = pack_data(mlc, schema, packet, &packet_ptr, &packet_remaining, &tokbuf, &CHILD_ERRMSG);
    RAISE_IF(pack_result == EXIT_FAIL, "\n%s", CHILD_ERRMSG)

    // mutate packet_size (will be used outside)
    *packet_size = packet_ptr - *packet;

    // Trim the output buffer to the exact size needed
    if (packet_remaining > 0) {
        *packet = (char*)realloc(*packet, *packet_size);
    }

    return pack_result;
}


// Take a morloc datastructure and convert it to MessagePack
int pack(const void* mlc, const char* schema_str, char** mpk, size_t* mpk_size, ERRMSG) {
    INT_RETURN_SETUP

    Schema* schema = parse_schema(&schema_str, &CHILD_ERRMSG);
    RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)

    int exit_code = pack_with_schema(mlc, schema, mpk, mpk_size, &CHILD_ERRMSG);
    RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG);

    return exit_code;
}



// nested msg_sizers
static size_t msg_size(const char* mgk, size_t mgk_size, const Schema* schema);
static size_t msg_size_r(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_bytes(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

static size_t msg_size_bytes(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t array_size = token->length;

    size_t str_idx = 0;
    while((array_size - str_idx) > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        str_idx += token->length;
    }
    return array_size + sizeof(Array);
}

static size_t msg_size_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t array_length = token->length;
    size_t size = sizeof(Array);
    for(size_t i = 0; i < array_length; i++){
        size += msg_size_r(schema, tokbuf, buf_ptr, buf_remaining, token);
    }
    return size;
}

static size_t msg_size_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    // parse the mesgpack tuple
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    assert(token->length == schema->size); 
    size_t size = 0;
    for(size_t i = 0; i < schema->size; i++){
        size += msg_size_r(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
    }
    return size;
}

static size_t msg_size_r(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    switch(schema->type){
      case MORLOC_NIL:
      case MORLOC_BOOL:
      case MORLOC_SINT8:
      case MORLOC_SINT16:
      case MORLOC_SINT32:
      case MORLOC_SINT64:
      case MORLOC_UINT8:
      case MORLOC_UINT16:
      case MORLOC_UINT32:
      case MORLOC_UINT64:
      case MORLOC_FLOAT32:
      case MORLOC_FLOAT64:
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        return schema->width;
      case MORLOC_STRING:
        return msg_size_bytes(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_ARRAY:
        return msg_size_array(schema->parameters[0], tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        return msg_size_tuple(schema, tokbuf, buf_ptr, buf_remaining, token);
      default:
        return 0;
    }
}

static size_t msg_size(const char* mgk, size_t mgk_size, const Schema* schema) {
    size_t buf_remaining = mgk_size;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;
    mpack_token_t token;
    return msg_size_r(schema, &tokbuf, &mgk, &buf_remaining, &token);
}



// terminal parsers
static EXIT_CODE parse_bool(        void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_nil(         void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_bytes(       void* mlc, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_int(    morloc_serial_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_float(  morloc_serial_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

// nested parsers
static EXIT_CODE parse_array( void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_tuple( void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_obj(   void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);

static EXIT_CODE parse_nil(void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    *((int8_t*)mlc) = (int8_t)0;
    return EXIT_PASS;
}

static EXIT_CODE parse_bool(void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    // boolean here needs to be uint8 since the C `bool` type is not guaranteed
    // to be 1 byte, it is likely typedefed to `int`, which is 32 bit.
    *((uint8_t*)mlc) = (uint8_t) mpack_unpack_boolean(*token) ? 1 : 0;
    return EXIT_PASS;
}

static EXIT_CODE parse_int(morloc_serial_type schema_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG){
    INT_RETURN_SETUP

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    switch(token->type){
      case MPACK_TOKEN_UINT:
        switch(schema_type){
          case MORLOC_UINT8:
            *((uint8_t*)mlc) = (uint8_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT16:
            *((uint16_t*)mlc) = (uint16_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT32:
            *((uint32_t*)mlc) = (uint32_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT64:
            *((uint64_t*)mlc) = (uint64_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT8:
            *((int8_t*)mlc) = (int8_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT16:
            *((int16_t*)mlc) = (int16_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT32:
            *((int32_t*)mlc) = (int32_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT64:
            *((int64_t*)mlc) = (int64_t)mpack_unpack_uint(*token);
            break;
          default:
            break;
        }
        break;
      case MPACK_TOKEN_SINT:
        switch(schema_type){
          case MORLOC_SINT8:
            *((int8_t*)mlc) = (int8_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT16:
            *((int16_t*)mlc) = (int16_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT32:
            *((int32_t*)mlc) = (int32_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT64:
            *((int64_t*)mlc) = (int64_t)mpack_unpack_sint(*token);
            break;
          default:
            break;
        }
        break;
      default:
        RAISE("Bad token %d\n", token->type);
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_float(
    morloc_serial_type schema_type,
    void* mlc,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token
){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    if(schema_type == MORLOC_FLOAT32){
      *(float*)mlc = (float)mpack_unpack_float(*token);
    } else {
      *(double*)mlc = (double)mpack_unpack_float(*token);
    }

    return EXIT_PASS;
}


static EXIT_CODE parse_bytes(
    void* mlc,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    Array* result = (Array*) mlc;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    result->size = token->length;

    result->data = abs2rel(*cursor, &CHILD_ERRMSG);
    RAISE_IF(result->data == RELNULL, "\n%s", CHILD_ERRMSG)

    *cursor = (char*)(*cursor) + result->size;

    size_t str_idx = 0;
    while((result->size - str_idx) > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);

        void* voidstar = rel2abs(result->data + str_idx, &CHILD_ERRMSG);
        RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)

        memcpy(
          voidstar,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_array(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    Array* result = (Array*) mlc;

    size_t element_size = schema->width;
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    result->size = token->length;

    result->data = abs2rel(*cursor, &CHILD_ERRMSG);
    RAISE_IF(result->data == RELNULL, "\n%s", CHILD_ERRMSG)

    *cursor = (char*)(*cursor) + result->size * element_size;

    for(size_t i = 0; i < result->size; i++){
        void* voidstar = rel2abs(result->data + i * element_size, &CHILD_ERRMSG);
        RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)

        int exit_code = parse_obj(voidstar, schema, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_tuple(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    size_t offset = 0;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);

    for(size_t i = 0; i < schema->size; i++){
        int exit_code = parse_obj((char*)mlc + offset, schema->parameters[i], cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        offset += schema->parameters[i]->width;
    }

    return EXIT_PASS;
}

static EXIT_CODE parse_obj(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    int child_retcode;
    switch(schema->type){
      case MORLOC_NIL:
        return parse_nil(mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL:
        return parse_bool(mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_SINT8:
      case MORLOC_SINT16:
      case MORLOC_SINT32:
      case MORLOC_SINT64:
      case MORLOC_UINT8:
      case MORLOC_UINT16:
      case MORLOC_UINT32:
      case MORLOC_UINT64:
        child_retcode = parse_int(schema->type, mlc, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_FLOAT32:
      case MORLOC_FLOAT64:
        return parse_float(schema->type, mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_STRING:
        child_retcode = parse_bytes(mlc, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_ARRAY:
        child_retcode = parse_array(mlc, schema->parameters[0], cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        child_retcode = parse_tuple(mlc, schema, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      default:
        RAISE("Failed to parse morloc type %d\n", schema->type)
    }
    return EXIT_PASS;
}

int unpack_with_schema(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr, ERRMSG) {
    INT_RETURN_SETUP

    // Pass once over the MessagePack data, calculating the allocation size
    size_t size = msg_size(mgk, mgk_size, schema);

    void* mlc = (void*)shmalloc(size, &CHILD_ERRMSG);
    RAISE_IF(mlc == NULL, "\n%s", CHILD_ERRMSG)

    // Use the existing unpack_with_schema function, but adapt it to the new prototype
    size_t buf_remaining = mgk_size;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;
    mpack_token_t token;

    void* cursor = (void*)((char*)mlc + schema->width);

    int exitcode = parse_obj(mlc, schema, &cursor, &tokbuf, &mgk, &buf_remaining, &token, &CHILD_ERRMSG);
    RAISE_IF(exitcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)

    *mlcptr = mlc;

    return exitcode;
}

// take MessagePack data and set a pointer to an in-memory data structure
int unpack(const char* mpk, size_t mpk_size, const char* schema_str, void** mlcptr, ERRMSG) {
    INT_RETURN_SETUP
    const Schema* schema = parse_schema(&schema_str, &CHILD_ERRMSG);
    RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)
    int exit_code = unpack_with_schema(mpk, mpk_size, schema, mlcptr, &CHILD_ERRMSG);
    RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
    return exit_code;
}

// }}} end Morloc pack support

// {{{ JSON support

// Function to escape a JSON string
static char* json_escape_string(const char* input, size_t input_len) {
    size_t output_len = 0;
    size_t i;

    // First pass: calculate the required length of the escaped string
    for (i = 0; i < input_len; i++) {
        switch (input[i]) {
            case '\"': case '\\': case '/': case '\b':
            case '\f': case '\n': case '\r': case '\t':
                output_len += 2;
                break;
            default:
                if ((unsigned char)input[i] < 32) {
                    output_len += 6; // \u00xx
                } else {
                    output_len++;
                }
        }
    }

    char* output = (char*)malloc(output_len + 1); // +1 for null terminator
    if (!output) return NULL;

    size_t j = 0;
    // Second pass: copy and escape characters
    for (i = 0; i < input_len; i++) {
        switch (input[i]) {
            case '\"': output[j++] = '\\'; output[j++] = '\"'; break;
            case '\\': output[j++] = '\\'; output[j++] = '\\'; break;
            case '/':  output[j++] = '\\'; output[j++] = '/';  break;
            case '\b': output[j++] = '\\'; output[j++] = 'b';  break;
            case '\f': output[j++] = '\\'; output[j++] = 'f';  break;
            case '\n': output[j++] = '\\'; output[j++] = 'n';  break;
            case '\r': output[j++] = '\\'; output[j++] = 'r';  break;
            case '\t': output[j++] = '\\'; output[j++] = 't';  break;
            default:
                if ((unsigned char)input[i] < 32) {
                    sprintf(&output[j], "\\u%04x", input[i]);
                    j += 6;
                } else {
                    output[j++] = input[i];
                }
        }
    }
    output[j] = '\0';

    return output;
}

// }}} end JSON support

// {{{ voidstar utilities

static bool print_voidstar_r(const void* voidstar, const Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    Array* array;
    const char* data;

    switch (schema->type) {
        case MORLOC_NIL:
            printf("null");
            break;
        case MORLOC_BOOL:
            printf(*(uint8_t*)voidstar ? "true" : "false");
            break;
        case MORLOC_UINT8:
            printf("%u", *(uint8_t*)voidstar);
            break;
        case MORLOC_UINT16:
            printf("%u", *(uint16_t*)voidstar);
            break;
        case MORLOC_UINT32:
            printf("%u", *(uint32_t*)voidstar);
            break;
        case MORLOC_UINT64:
            printf("%lu", *(uint64_t*)voidstar);
            break;
        case MORLOC_SINT8:
            printf("%d", *(int8_t*)voidstar);
            break;
        case MORLOC_SINT16:
            printf("%d", *(int16_t*)voidstar);
            break;
        case MORLOC_SINT32:
            printf("%d", *(int32_t*)voidstar);
            break;
        case MORLOC_SINT64:
            printf("%ld", *(int64_t*)voidstar);
            break;
        case MORLOC_FLOAT32:
            printf("%.7g", *(float*)voidstar);
            break;
        case MORLOC_FLOAT64:
            printf("%.15g", *(double*)voidstar);
            break;
        case MORLOC_STRING:
            {
                array = (Array*)voidstar;
                data = (const char*)rel2abs(array->data, &CHILD_ERRMSG);
                RAISE_IF(data == NULL, "\n%s", CHILD_ERRMSG)

                char* escaped_string = json_escape_string(data, array->size);
                if (escaped_string) {
                    printf("\"%s\"", escaped_string);
                    FREE(escaped_string);
                } else {
                    RAISE("Memory allocation failed for string escaping");
                }
            }
            break;
        case MORLOC_ARRAY:
            {
                array = (Array*)voidstar;
                data = (const char*)rel2abs(array->data, &CHILD_ERRMSG);
                RAISE_IF(data == NULL, "\n%s", CHILD_ERRMSG)

                printf("[");
                for (size_t i = 0; i < array->size; i++) {
                    if (i > 0) printf(",");
                    bool success = print_voidstar_r(data + i * schema->parameters[0]->width, schema->parameters[0], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("]");
            }
            break;
        case MORLOC_TUPLE:
            {
                printf("[");
                for (size_t i = 0; i < schema->size; i++) {
                    if (i > 0) printf(",");
                    bool success = print_voidstar_r((char*)voidstar + schema->offsets[i], schema->parameters[i], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("]");
            }
            break;
        case MORLOC_MAP:
            {
                printf("{");
                for (size_t i = 0; i < schema->size; i++) {
                    if (i > 0) printf(",");
                    printf("\"%s\":", schema->keys[i]);
                    bool success = print_voidstar_r((char*)voidstar + schema->offsets[i], schema->parameters[i], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("}");
            }
            break;
        default:
            RAISE("Unexpected morloc type");
    }

    return true;
}

bool print_voidstar(const void* voidstar, const Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    // print JSON with no spaces
    bool success = print_voidstar_r(voidstar, schema, &CHILD_ERRMSG);
    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)

    // add terminal newline
    printf("\n");

    return success;
}

// }}} end voidstar utilities

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

// call: send zero or more data packets as arguments to manifold midx
typedef struct __attribute__((packed)) packet_command_call_s {
    command_type_t type; // identifier for resolving unions
    uint8_t padding[3]; // padding in the middle to align midx
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

// The union of the 8-byte commands above
typedef union packet_command_u {
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
  // arbitrary extra information. Not currently used, but the <mode> field and
  // special future command types may use this space for custom ends.
  uint32_t offset;
  // length of the main payload data
  uint64_t length;
} morloc_packet_header_t;

static_assert(
    sizeof(morloc_packet_header_t) == 32,
    "Header size mismatch!"
);

// }}}

// {{{ morloc packet API

typedef struct morloc_call_s {
    uint32_t midx;
    uint8_t** args;
    size_t nargs;
} morloc_call_t;

morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG){
    PTR_RETURN_SETUP(morloc_packet_header_t)

    RAISE_IF(msg == NULL, "Cannot make packet from NULL pointer")

    morloc_packet_header_t* header = (morloc_packet_header_t*) msg;
    RAISE_IF(header->magic != MORLOC_PACKET_MAGIC, "Malformed morloc packet")

    return header;
}


size_t morloc_packet_size_from_header(const morloc_packet_header_t* header){
    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}

size_t morloc_packet_size(const uint8_t* packet, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0);

    morloc_packet_header_t* header = read_morloc_packet_header(packet, &CHILD_ERRMSG);
    RAISE_IF(header == NULL, "\n%s", CHILD_ERRMSG)

    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}


// Set the packet header at the first 32 bytes of a data block
static void set_morloc_packet_header(
    uint8_t* data,
    packet_command_t cmd,
    uint32_t offset,
    uint64_t length
){
    morloc_packet_header_t* header = (morloc_packet_header_t*) data;
    header->magic = MORLOC_PACKET_MAGIC;
    header->plain = THIS_PLAIN;
    header->version = THIS_VERSION;
    header->flavor = DEFAULT_FLAVOR;
    header->mode = DEFAULT_MODE;
    header->command = cmd;
    header->offset = offset;
    header->length = length;
}


static uint8_t* make_morloc_data_packet(
    const uint8_t* data,
    size_t data_length,
    const uint8_t* metadata,
    size_t metadata_length,
    uint8_t src,
    uint8_t fmt,
    uint8_t cmpr,
    uint8_t encr,
    uint8_t status
){

  size_t packet_length = sizeof(morloc_packet_header_t) + metadata_length + data_length;
  uint8_t* packet = (uint8_t*)malloc(packet_length * sizeof(uint8_t));

  packet_command_t cmd = {
    .data = {
        .type = PACKET_TYPE_DATA,
        .source = src,
        .format = fmt,
        .compression = cmpr,
        .encryption = encr,
        .status = status,
        .padding = { 0, 0 }
    }
  };

  // generate the header
  set_morloc_packet_header(packet, cmd, metadata_length, data_length);

  if (metadata != NULL && metadata_length > 0) {
      memcpy(packet + sizeof(morloc_packet_header_t), metadata, metadata_length);
  }

  if (data != NULL && data_length > 0){
      memcpy(packet + sizeof(morloc_packet_header_t) + metadata_length, data, data_length);
  }

  return packet;
}


// Make a data packet from a relative pointer to shared memory data
uint8_t* make_relptr_data_packet(relptr_t ptr, const Schema* schema){
    uint8_t* packet = make_morloc_data_packet(
      NULL, sizeof(ssize_t), // send no payload, only allocate space
      NULL, 0, // no metadata
      PACKET_SOURCE_RPTR,
      PACKET_FORMAT_VOIDSTAR,
      PACKET_COMPRESSION_NONE,
      PACKET_ENCRYPTION_NONE,
      PACKET_STATUS_PASS
    );

    // set the payload
    *((ssize_t*)(packet + sizeof(morloc_packet_header_t))) = ptr;

    return packet;
}


uint8_t* make_fail_packet(const char* failure_message){
  return make_morloc_data_packet(
    (const uint8_t*)failure_message,
    strlen(failure_message),
    NULL, 0,
    PACKET_SOURCE_MESG,
    PACKET_FORMAT_TEXT,
    PACKET_COMPRESSION_NONE,
    PACKET_ENCRYPTION_NONE,
    PACKET_STATUS_FAIL
  );
}


uint8_t* read_binary_file(const char* filename, size_t* file_size, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    RAISE_IF(filename == NULL, "Found NULL filename")

    uint8_t* msg = NULL;
    size_t read_size = 0;
    size_t file_size_long = 0;
    FILE* file = NULL;

    file = fopen(filename, "rb");
    RAISE_IF(file == NULL, "Failed to open file '%s'", filename)

    int return_code = fseek(file, 0, SEEK_END);
    RAISE_IF_WITH(
        return_code != 0,
        fclose(file),
        "Failed to seek to end of file"
    )

    file_size_long = ftell(file);
    RAISE_IF_WITH(
        file_size_long < 0,
        fclose(file),
        "Failed to determine the size of file '%s'",
        filename
    )

    RAISE_IF_WITH(
        file_size_long == 0,
        fclose(file),
        "The file '%s' is empty",
        filename
    )

    // If the file is too large to fit in memory, we will need to stream it.
    // TODO: add streaming support
    RAISE_IF(
        file_size_long > SIZE_MAX,
        "The file '%s' is %zu bytes in length which exceeds the maximum allocatable size",
        filename,
        file_size_long
    )

    // NOTE, do not cast this to size_t earlier since it may overflow
    *file_size = (size_t)file_size_long;

    rewind(file);

    msg = (uint8_t*)malloc(*file_size);
    RAISE_IF_WITH(
        msg == NULL,
        fclose(file),
        "Failed to allocate %zu bytes",
        *file_size
    )

    read_size = fread(msg, 1, *file_size, file);
    if(read_size != *file_size){
        free(msg);
        fclose(file);
        RAISE("Read %zu bytes (expected %zu)", read_size, *file_size)
    }

    fclose(file);
    return msg;
}

// mutates the given errmsg to the error value of the data packet
//
// if the data packet cannot be read, false is returned and errmsg is set to the
// reason for the reading failure
//
// if the data packet can be read, but it does not have a failing state, or is
// not a data packet, then true is returned and errmsg is set to an empty string
// (just a \0 character)
//
// if the data packet can be read, and it is a failing data packet, then true is
// returned and errmsg is copied from the packet (this will need to be freed)
bool get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG){
    BOOL_RETURN_SETUP

    morloc_packet_header_t* header = read_morloc_packet_header(data, &CHILD_ERRMSG);
    RAISE_IF(header == NULL, "Failed to read packet:\n%s", CHILD_ERRMSG);

    if (header->command.data.status == PACKET_STATUS_FAIL) {
        char* new_packet_error = (char*)calloc(header->length + 1, sizeof(char));
        RAISE_IF(new_packet_error == NULL, "Failed to allocate error message"); 

        char* packet_data = (char*)data + sizeof(morloc_packet_header_t) + (size_t)header->offset;
        void* copy_ptr = memcpy(new_packet_error, packet_data, header->length);
        RAISE_IF(copy_ptr == NULL, "Failed to copy error message to packet")

        FREE(new_packet_error)
        return true;
    }

    return false;
}


// Opens a data packet returning a pointer to a packet
//  * If packet has failing status, it is returned unchanged (error propagation)
//  * If packet handling fails, the fail_packet is defined and NULL is returned
//  * Else an absolute pointer to voidstar data is returned
uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    uint8_t source;
    uint8_t format;
    uint8_t status;

    void* voidstar = NULL;

    morloc_packet_header_t* header = read_morloc_packet_header(data, &CHILD_ERRMSG);
    RAISE_IF(header == NULL, "Failed to read packet:\n%s", CHILD_ERRMSG)

    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected a data packet");

    source = header->command.data.source;
    format = header->command.data.format;
    status = header->command.data.status;

    if (status == PACKET_STATUS_FAIL) {
        bool passed = get_morloc_data_packet_error_message(data, &CHILD_ERRMSG);
        if(passed){
            RAISE("Propagating:\n%s", CHILD_ERRMSG)
        } else {
            RAISE("Empty error packet")
        }
    }

    switch (source) {
        case PACKET_SOURCE_MESG:
            if (format == PACKET_FORMAT_MSGPACK) {
                int unpack_result = unpack_with_schema((
                    const char*)data + sizeof(morloc_packet_header_t) + header->offset,
                    header->length,
                    schema,
                    &voidstar,
                    &CHILD_ERRMSG
                );
                RAISE_IF(unpack_result == EXIT_FAIL, "Failed to parse MessagePacket data:\n%s", CHILD_ERRMSG);
            } else {
                RAISE("Invalid format from mesg: %uhh", format);
            }
            break;

        case PACKET_SOURCE_FILE:
            switch (format) {
                case PACKET_FORMAT_MSGPACK: {
                    char* filename = strndup((char*)data + sizeof(morloc_packet_header_t), MAX_FILENAME_SIZE);
                    size_t file_size;
                    uint8_t* msg = read_binary_file(filename, &file_size, &CHILD_ERRMSG);
                    RAISE_IF(msg == NULL, "Failed to open MessagePack file: %s", CHILD_ERRMSG)

                    // Unpack the binary buffer using the schema
                    int unpack_result = unpack_with_schema((const char*)msg, file_size, schema, &voidstar, &CHILD_ERRMSG);
                    RAISE_IF(unpack_result == EXIT_FAIL, "\n%s", CHILD_ERRMSG)

                    FREE(msg);
                    RAISE_IF(unpack_result != 0, "Failed to unpack data from file")
                }
            }
            RAISE("Invalid format from file: %uhh", format);
            return NULL;

        case PACKET_SOURCE_RPTR:
            if (format == PACKET_FORMAT_VOIDSTAR) {
                // This packet should contain a relative pointer as its payload
                size_t relptr = *(size_t*)(data + header->offset + sizeof(morloc_packet_header_t));
                voidstar = rel2abs(relptr, &CHILD_ERRMSG);
                RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)
            } else {
                RAISE("For RPTR source, expected voidstar format");
                return NULL;
            }
            break;

        default:
            RAISE("Invalid source");
    }

    return (uint8_t*)voidstar;
}

uint8_t* make_morloc_call_packet(uint64_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    size_t data_length = 0;
    uint32_t offset = 0;

    for(size_t i = 0; i < nargs; i++){
        morloc_packet_header_t* arg = read_morloc_packet_header(arg_packets[i], &CHILD_ERRMSG);
        RAISE_IF(arg == NULL, "\n%s", CHILD_ERRMSG)

        data_length += sizeof(morloc_packet_header_t) + (size_t)arg->offset + (size_t)arg->length;
    }

    size_t packet_length = data_length + offset + sizeof(morloc_packet_header_t);

    uint8_t* data = (uint8_t*)malloc(packet_length * sizeof(uint8_t));

    packet_command_t cmd = {
      .call = {
        .type = PACKET_TYPE_CALL,
        .padding = {0, 0, 0},
        .midx = (uint32_t)midx,
      }
    };

    set_morloc_packet_header(data, cmd, offset, data_length);

    size_t arg_start = sizeof(morloc_packet_header_t) + offset;
    for(size_t i = 0; i < nargs; i++){
      morloc_packet_header_t* arg = read_morloc_packet_header(arg_packets[i], &CHILD_ERRMSG);
      RAISE_IF(arg == NULL, "\n%s", CHILD_ERRMSG)
      size_t arg_length = morloc_packet_size_from_header(arg);
      memcpy(data + arg_start, arg, arg_length);
      arg_start += arg_length;
    }

    return data;
}



morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(morloc_call_t)

    morloc_call_t* call = (morloc_call_t*)malloc(sizeof(morloc_call_t));

    morloc_packet_header_t* header = read_morloc_packet_header(packet, &CHILD_ERRMSG);
    RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read call packet header:\n%s", CHILD_ERRMSG)
    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_CALL, "Expected packet to be a call")

    call->midx = header->command.call.midx;
    call->nargs = 0;
    call->args = NULL;

    size_t pos = sizeof(morloc_packet_header_t) + header->offset;
    while(pos < header->length){
        pos += morloc_packet_size(packet + pos, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read argument #%zu\n%s", call->nargs, CHILD_ERRMSG)
        call->nargs++;
    }

    call->args = (uint8_t**)calloc(call->nargs, sizeof(uint8_t*));
    pos = sizeof(morloc_packet_header_t) + header->offset;
    for(size_t i = 0; i < call->nargs; i++){
        morloc_packet_header_t* arg_header = read_morloc_packet_header(packet + pos, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read call argument #%zu:\n%s", i, CHILD_ERRMSG)
        RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected argument to be a data packet")

        size_t arg_packet_size = morloc_packet_size_from_header(arg_header);

        // Copy the argument
        // We alternatively we could avoid this copy and pass the pointer to the
        // original memory. This would improve performance, but would be less
        // safe. I'll leave this optimization for later.
        call->args[i] = (uint8_t*)malloc(arg_packet_size * sizeof(uint8_t));
        memcpy(call->args[i], packet + pos, arg_packet_size);

        pos += arg_packet_size;
    }

    return call;
}

// }}} end packet support

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
static uint64_t mix(uint64_t a, uint64_t b) {
    a ^= b * PRIME64_1;
    a = (a << 31) | (a >> (64 - 31)); // Rotate
    a *= PRIME64_2;
    return a;
}


static bool schema_is_fixed_width(const Schema* schema){
    switch(schema->type){
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            return false;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            for(size_t i = 0; i < schema->size; i++){
                if (! schema_is_fixed_width(schema->parameters[i])){
                    return false;
                }
            }
            break;
        default:
            return true;
            break;
    }
    return true;
}


static uint64_t hash_voidstar(absptr_t data, const Schema* schema, uint64_t seed, ERRMSG){
    VAL_RETURN_SETUP(uint64_t, 0)

    uint64_t hash = seed;
    switch(schema->type){
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                size_t element_width = schema->parameters[0]->width;

                uint8_t* element_data = (uint8_t*)rel2abs(array->data, &CHILD_ERRMSG);
                RAISE_IF(element_data == NULL, "\n%s", CHILD_ERRMSG)

                if (schema_is_fixed_width(schema)){
                    size_t array_size = element_width * array->size;
                    hash = XXH64(element_data, array_size, seed);
                } else {
                    for(size_t i = 0; i < array->size; i++){
                        hash = hash_voidstar(element_data + i * element_width, schema->parameters[0], hash, &CHILD_ERRMSG);
                        RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG)
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                if (schema_is_fixed_width(schema)){
                    hash = XXH64(data, schema->width, seed);
                } else {
                    uint8_t* element_data = (uint8_t*)data;
                    for(size_t i = 0; i < schema->size; i++){
                        hash = hash_voidstar(element_data + schema->offsets[i], schema->parameters[i], hash, &CHILD_ERRMSG);
                        RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG);
                    }
                }
            }
            break;
        default:
            hash = XXH64((uint8_t*)data, schema->width, seed);
            break;
    }
    return hash;
}


// Hash a morloc packet
bool hash_morloc_packet(const uint8_t* packet, const Schema* schema, uint64_t seed, uint64_t* hash, ERRMSG){
    BOOL_RETURN_SETUP

    morloc_packet_header_t* header = (morloc_packet_header_t*)packet;
    *hash = 0; // 0 softly represents a failed hash, though it could appear by chance
    uint8_t command_type = header->command.cmd_type.type;

    if (command_type == PACKET_TYPE_CALL){
        uint32_t midx = header->command.call.midx;
        *hash = mix(seed, (uint64_t)midx);
        const uint8_t* arg_data = packet + sizeof(morloc_packet_header_t) + (size_t)header->offset;
        size_t arg_start = 0;
        while(arg_start < header->length){
            uint64_t arg_hash = hash_voidstar((void*)(arg_data + arg_start), schema, 0, &CHILD_ERRMSG);
            RAISE_IF(CHILD_ERRMSG != NULL, "Failed to hash call packet argument\n%s", CHILD_ERRMSG)
            *hash = mix(*hash, arg_hash);
        }
    } else if (command_type == PACKET_TYPE_DATA){
        uint8_t* voidstar = get_morloc_data_packet_value(packet + sizeof(morloc_packet_header_t) + header->offset, schema, &CHILD_ERRMSG);
        RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)
        *hash = hash_voidstar((void*)packet, schema, seed, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to hash data packet\n%s", CHILD_ERRMSG);
    } else {
        RAISE("Cannot hash packet with command %uhh", command_type)
    }

    return true; // success
}


static bool make_cache_filename(uint64_t key, const char* cache_path, char* buf, size_t buf_size) {
    // Format the filename with the key in hexadecimal
    //  * It is always padded to 16 characters
    //  * The PRIx64 macro from inttypes.h ensures portability since the
    //    uint64_t type may be aliased to different 64 bit types on
    //    different systems (e.g., unsigned long or unsigned long long)
    int written = snprintf(buf, buf_size, "%s/%016" PRIx64, cache_path, key);

    // Check if snprintf succeeded and didn't truncate
    return (written > 0 && (size_t)written < buf_size);
}


// Sends data to cache given an integer key. The main use case is caching the
// return values from remote calls. In thise case, the key will be the hash of
// the call which accounts for all inputs and the code the operates on
// it. Importantly, the key is NOT the hash of this return value (because we
// don't know the result before we run the computation).
//
// If the packet is successfully cached, return the cache filename
// Else return NULL
char* put_cache_packet(const uint8_t* packet, uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)

    size_t size = morloc_packet_size(packet, &CHILD_ERRMSG);
    RAISE_IF(size == 0, "\n%s", CHILD_ERRMSG)

    // Generate the cache filename
    char filename[MAX_FILENAME_SIZE];
    RAISE_IF(!make_cache_filename(key, cache_path, filename, sizeof(filename)), "Failed to make cache filename")

    // Open the file to store the binary packet data
    FILE* file = fopen(filename, "wb");
    RAISE_IF(!file, "Failed to open file '%s'", filename)

    // Write the packet data to the file
    size_t written = fwrite(packet, 1, size, file);
    RAISE_IF_WITH(
        written != size,
        fclose(file),
        "Failed to write to cache file '%s'",
        filename
    )

    // Ensure all data is flushed and check for write errors
    int exit_code = fclose(file);
    RAISE_IF(exit_code != 0, "Failed to close cache file '%s'", filename)

    return strdup(filename);
}


// Get a cached packet given the key (usually a hash)
uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    // Generate the cache filename
    char filename[MAX_FILENAME_SIZE];

    RAISE_IF(
        !make_cache_filename(key, cache_path, filename, sizeof(filename)),
        "Failed to make cache filename"
    )

    // Read the binary file into memory
    size_t file_size;
    uint8_t* data = read_binary_file(filename, &file_size, &CHILD_ERRMSG);
    RAISE_IF(data == NULL, "Failed to open cache file: \n%s", CHILD_ERRMSG)

    return data;
}


// Deletes a cached packet given a key
bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    BOOL_RETURN_SETUP

    // Generate the cache filename
    char filename[MAX_FILENAME_SIZE];

    RAISE_IF(
        !make_cache_filename(key, cache_path, filename, sizeof(filename)),
        "Failed to make cache filename"
    )

    // Attempt to delete the file
    RAISE_IF(
        unlink(filename) != 0,
        "Failed to delete cache file '%s'",
        filename
    )

    return true; // deletion success
}

// Checks if a cached packet exists given a key
//
// If a cached packet exists, return the filename
// Else return NULL
char* check_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)

    // Generate the cache filename
    char filename[MAX_FILENAME_SIZE];
    bool success = make_cache_filename(key, cache_path, filename, sizeof(filename));
    RAISE_IF(!success, "Failed to make cache filename")

    // check if the file exists
    struct stat file_stat;
    if (stat(filename, &file_stat) == 0) {
        return strdup(filename); // File exists
    } else {
        // NOTE, this is NOT an error, it just means that no cache file exists
        return NULL;
    }
}

// }}} end hash support

// prolly bring this back
// #ifdef SLURM_SUPPORT

// {{{ slurm support

// For each field, -1 indicates undefined
typedef struct resources {
  int memory; // in Gb
  int time; // in seconds, for 32bit int, you are limited to 68 years
  int cpus;
  int gpus;
} resources;

#define DEFAULT_XXHASH_SEED 0

// Parse the arguments of a morloc call packet.
//
// This function mutates the `args ` and `nargs` arguments to store the call
// arguments and counts (respectively). `args` contains pointers to locations
// in the original `packet` array, so `packet` must not be freed until after
// `args` is freed.
bool parse_morloc_call_arguments(
    uint8_t* packet, // input call packet
    uint8_t** args, // pointer to vector of arguments (MUTATED)
    size_t* nargs, // pointer to number of arguments (MUTATED)
    ERRMSG
){
    BOOL_RETURN_SETUP

    *nargs = 0;

    morloc_packet_header_t* header = (morloc_packet_header_t*)packet;
    size_t packet_size = morloc_packet_size_from_header(header);

    RAISE_IF(
        header->command.cmd_type.type != PACKET_TYPE_CALL,
        "Unexpected packet type (BUG)"
    )

    morloc_packet_header_t* arg_header;
    size_t pos = sizeof(morloc_packet_header_t) + header->offset;
    while(pos < packet_size){
        arg_header = (morloc_packet_header_t*)(packet + pos);
        pos += sizeof(morloc_packet_header_t) + arg_header->offset + arg_header->length;
        *nargs += 1;
    }

    *args = (uint8_t*)malloc(*nargs * sizeof(uint8_t*));
    RAISE_IF(*args == NULL, "Failed to allocate memory for argument vector")

    pos = sizeof(morloc_packet_header_t) + (size_t)header->offset;
    for(size_t i = 0; i < *nargs; i++){
        uint8_t* arg = packet + pos;
        morloc_packet_header_t* arg_header = (morloc_packet_header_t*)arg;
        args[i] = packet + pos;
        pos += sizeof(morloc_packet_header_t) + arg_header->offset + arg_header->length;
    }

    return true;
}

// Before calling this function, every argument must be converted to a packet
//   * if the data is native, then it should be converted
uint8_t* remoteCall(
    int midx,
    const char* socket_path, // domain socket file for target pool
    const char* cache_path, // path where args and results will be written
    const resources* res, // required system resources (mem, cpus, etc)
    const Schema** arg_schemas, // schemas for each argument
    const uint8_t** arg_packets, // voidstar for each argument
    size_t nargs, // number of arguments
    ERRMSG
){
    PTR_RETURN_SETUP(uint8_t)

    uint64_t seed = (uint64_t) midx;

    // The function hash determins the output file name on the remote node and is
    // used to determine if this computation has already been run. The function
    // hash **should** be unique to a function and its inputs.
    //
    // TODO: Actually hash the function code, not just the manifold id.
    uint64_t function_hash = mix(seed, DEFAULT_XXHASH_SEED);

    uint8_t** new_arg_packets = (uint8_t**)malloc(nargs * sizeof(uint8_t*));

    // hash voidstar data for every argument
    for(size_t i = 0; i < nargs; i++){
        morloc_packet_header_t* arg_header = read_morloc_packet_header(arg_packets[i], &CHILD_ERRMSG);
        RAISE_IF(arg_header == NULL, "Malformed argument packet:\n%s", CHILD_ERRMSG)

        uint8_t* arg_voidstar = get_morloc_data_packet_value(arg_packets[i], arg_schemas[i], &CHILD_ERRMSG);
        RAISE_IF(arg_voidstar == NULL, "Failed to read argument #%zu:\n%s", i, CHILD_ERRMSG)

        uint64_t arg_hash = hash_voidstar(arg_voidstar, arg_schemas[i], DEFAULT_XXHASH_SEED, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to hash data packet\n%s", CHILD_ERRMSG)

        function_hash = mix(function_hash, arg_hash);

        char* arg_cache_filename = check_cache_packet(arg_hash, cache_path, &CHILD_ERRMSG);
        if(arg_cache_filename == NULL){
            CHILD_ERRMSG = NULL; // ignore error, if it failed, remake the cache
            arg_cache_filename = put_cache_packet(arg_voidstar, arg_hash, cache_path, &CHILD_ERRMSG);
            RAISE_IF(
                arg_cache_filename == NULL,
                "Failed to make argument hash:\n%s",
                CHILD_ERRMSG
            )
        }

        new_arg_packets[i] = make_morloc_data_packet(
          (uint8_t*)arg_cache_filename,
          strlen(arg_cache_filename),
          NULL, 0, // no metadata yet
          PACKET_SOURCE_FILE,
          PACKET_FORMAT_VOIDSTAR,
          PACKET_COMPRESSION_NONE,
          PACKET_ENCRYPTION_NONE,
          PACKET_STATUS_PASS
        );
    }

    uint8_t* call_packet = NULL;
    char* result_cache_filename = check_cache_packet(function_hash, cache_path, &CHILD_ERRMSG);
    if(result_cache_filename == NULL){
        CHILD_ERRMSG = NULL; // ignore error, if it failed, remake the cache
        // return result is not cached, so we do the operation
        call_packet = make_morloc_call_packet(midx, (const uint8_t**)new_arg_packets, nargs, &CHILD_ERRMSG);
        RAISE_IF(call_packet == NULL, "Failed to make call packet:\n%s", CHILD_ERRMSG)
    } else {
        // return result is cached, so load the cache and go
        uint8_t* final_result = get_cache_packet(function_hash, cache_path, &CHILD_ERRMSG);
        RAISE_IF(
            final_result == NULL,
            "Failed to open cached result at %s:\n%s",
            result_cache_filename,
            CHILD_ERRMSG
        )
        return final_result;
    }

    morloc_packet_header_t* call_header = read_morloc_packet_header(call_packet, &CHILD_ERRMSG);
    RAISE_IF(call_header == NULL, "Malformed call packet:\n%s", CHILD_ERRMSG)

    // // Note that this is not the same as the result hash, the remote compute
    // // node will load this packet as a call to run the job and will then write
    // // the results to the result hash cache.
    // uint64_t call_packet_hash_code = XXH64(call_packet, morloc_packet_size(call_header), DEFAULT_XXHASH_SEED);
    //
    // // write the call packet to cache
    //
    // // create the SLURM script
    // //   * call the remote nexus with the call packet
    // //   * write results to result_cache_filename
    //
    // // run the SLURM script
    //
    // // wait for the job to finish (looping every second or so)
    //
    // // when the job completes, create a packet with the `result_cache_filename`
    // // file and return
    
    return NULL;

}
    // // use the job description object to start a new SLURM job where the
    // // script calls nexus.py with three new arguments:
    // //   1. `--packet <packet_path>` - read from packet file
    // //   2. `--packet-lang <lang>` - send to this language pool
    // //   3. `--output raw` - write raw binary packet
    // //   4. `--output-file <file>` - write output to this file
    // // wait for the job to finish, then open the output packet
    // // if the packet is failing, delete the cached return value
    // // return the packet
    //
    //
    // job_descriptor_msg_t job_desc;
    // submit_response_msg_t *submit_response = NULL;
    //
    // slurm_init_job_desc_msg(&job_desc);
    //
    // job_desc.name = "my_job";
    // job_desc.partition = "standard";
    // job_desc.min_cpus = 1;
    // job_desc.min_memory = 1024;
    // job_desc.time_limit = 60;
    // job_desc.script = "#!/bin/bash\necho Hello, Slurm!";
    //
    // if (slurm_submit_batch_job(&job_desc, &submit_response) != SLURM_SUCCESS) {
    //     fprintf(stderr, "Job submission failed: %s\n", slurm_strerror(slurm_get_errno()));
    //     return 1;
    // }
    //
    // printf("Job submitted with ID %u\n", submit_response->job_id);
    // slurm_free_submit_response_response_msg(submit_response);
    //
    // // get message


// }}} end slurm support

// #endif // ending SLURM_SUPPORT

#endif // ending __MORLOC_H__
