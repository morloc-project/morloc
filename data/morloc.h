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

// return true if file exists (may or may not be openable)
bool file_exists(const char *filename) {
    return access(filename, F_OK) == 0; // F_OK checks for existence
}


// make a directory recursively (like `mkdir -p`)
int mkdir_p(const char *path, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)
    char *tmp = NULL;
    char *p = NULL;
    size_t len;
    int ret = -1;
    int const_err = 0;

    if (path == NULL || *path == '\0') {
        const_err = EINVAL;
        goto cleanup;
    }

    len = strlen(path);
    tmp = (char*)calloc(len + 2, sizeof(char)); // +2 for possible '/' and '\0'
    if (tmp == NULL) {
        const_err = errno;
        goto cleanup;
    }
    strcpy(tmp, path);

    // Add trailing slash if not present
    if (tmp[len - 1] != '/') {
        tmp[len] = '/';
        tmp[len + 1] = '\0';
        len++;
    }

    // Iterate and create directories
    for (p = tmp + 1; *p; p++) {
        if (*p == '/') {
            *p = '\0';
            if (mkdir(tmp, 0777) != 0) {
                if (errno != EEXIST) {
                    const_err = errno;
                    goto cleanup;
                }
            }
            *p = '/';
        }
    }
    // Create the final directory (in case path didn't end with '/')
    if (mkdir(tmp, 0777) != 0) {
        if (errno != EEXIST) {
            const_err = errno;
            goto cleanup;
        }
    }
    ret = 0;

cleanup:
    FREE(tmp);
    RAISE_IF(ret == -1, "mkdir_p('%s') failed: %s", path, strerror(const_err))
    return ret;
}



// Recursively delete a directory and its contents
void delete_directory(const char* path) {
    // open a directory stream
    DIR* dir = opendir(path);
    if (dir == NULL) {
        perror("Failed to tmpdir");
        return;
    }

    struct dirent* entry;
    char filepath[PATH_MAX];

    // iterate through all files in the directory
    while ((entry = readdir(dir)) != NULL) {
        // Skip "." and ".."
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        snprintf(filepath, sizeof(filepath), "%s/%s", path, entry->d_name);

        struct stat statbuf;
        if (stat(filepath, &statbuf) == -1) {
            perror("Failed to stat file");
            continue;
        }

        if (S_ISDIR(statbuf.st_mode)) {
            // recursively delete subdirectories
            delete_directory(filepath);
        } else {
            // delete files
            if (unlink(filepath) == -1) {
                perror("Failed to delete file");
            }
        }
    }

    closedir(dir);

    // Delete the directory itself
    if (rmdir(path) == -1) {
        perror("Failed to delete directory");
    }
}

// Check if a string has a given suffix
bool has_suffix(const char* x, const char* suffix){
    if( x == NULL || suffix == NULL){
        return false;
    }

    size_t x_len = strlen(x);
    size_t suffix_len = strlen(suffix);

    return (x_len >= suffix_len) &&
           strcmp(x + x_len - suffix_len, suffix) == 0;
}


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
char* dirname(char* path){
    static char dot[] = ".";
    char *last_slash;

    if (!path || !*path) return dot;

    /* Remove trailing slashes */
    last_slash = path + strlen(path) - 1;
    while (last_slash > path && *last_slash == '/') *last_slash-- = '\0';

    /* Find last slash */
    last_slash = strrchr(path, '/');

    if (!last_slash) return dot;
    if (last_slash == path) *(path+1) = '\0';  // Root case
    else *last_slash = '\0';  // Normal case

    return path;
}


int write_atomic(const char* filename, const uint8_t* data, size_t size, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)

    char tmp_path[MAX_FILENAME_SIZE];;
    FILE* file = NULL;
    int fd = -1;
    int ret = -1;
    int last_errno;
    int tmp_errno;
    char* file_dirpath = NULL;

    char *file_dirname = NULL;
    int dirfd = -1;

    // Validate input
    if (filename == NULL || (data == NULL && size != 0)) {
        errno = EINVAL;
        goto cleanup;
    }

    // Extract directory path for syncing
    if ((file_dirpath = strdup(filename)) == NULL) {
        goto cleanup;
    }
    file_dirname = dirname(file_dirpath);

    snprintf(tmp_path, sizeof(tmp_path), "%s/morloc-tmp_XXXXXX", file_dirname);

    // Create secure temp file
    if ((fd = mkstemp(tmp_path)) == -1) {
        goto cleanup;
    }

    // Convert to FILE* stream
    if ((file = fdopen(fd, "wb")) == NULL) {
        goto cleanup;
    }
    fd = -1; // Prevent double-close

    // Write data with verification
    if (fwrite(data, 1, size, file) != size) {
        goto cleanup;
    }

    // Critical data persistence sequence
    if (fflush(file) != 0) goto cleanup;
    if (fsync(fileno(file)) == -1) goto cleanup;

    // Close file properly before rename
    if (fclose(file) != 0) {
        file = NULL;
        goto cleanup;
    }
    file = NULL;

    // Atomic commit with directory sync
    if (rename(tmp_path, filename) == -1) {
        goto cleanup;
    }


    // Sync parent directory
    dirfd = open(file_dirname, O_RDONLY | O_DIRECTORY);
    if (dirfd == -1) {
        goto cleanup;
    }
    if (fsync(dirfd) == -1) {
        tmp_errno = errno;
        close(dirfd);
        errno = tmp_errno;
        goto cleanup;
    }
    close(dirfd);

    ret = 0;

cleanup:
    last_errno = errno;
    FREE(file_dirpath)
    if (file) fclose(file);
    if (ret == -1) unlink(tmp_path);
    RAISE_IF(ret == -1, "Atomic write to '%s' failed: %s", filename, strerror(last_errno))
    return ret;
}


// Write a given number of bytes to STDOUT
int print_binary(const char *buf, size_t count, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)
    size_t total_written = 0;
    ssize_t written;

    while (total_written < count) {
        written = write(STDOUT_FILENO, buf + total_written, count - total_written);
        if (written < 0) {
            RAISE("Failed to print data");
        }
        total_written += (size_t)written;
    }
    return 0;
}


uint8_t* read_binary_fd(FILE* file, size_t* file_size, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    uint8_t* msg = NULL;
    size_t read_size = 0;
    size_t file_size_long = 0;
    size_t allocated_size = 0;
    const size_t chunk_size = 0xffff;  // 64KB chunks

    // First attempt to use seek-based size detection
    if (fseek(file, 0, SEEK_END) == 0) {
        file_size_long = ftell(file);
        if (file_size_long > 0) {
            rewind(file);
            // Proceed with normal file handling
            RAISE_IF(file_size_long > SIZE_MAX,
                   "File too large (%zu bytes)", file_size_long)
            *file_size = (size_t)file_size_long;
            msg = (uint8_t*)malloc(*file_size);
            RAISE_IF(msg == NULL, "Failed to allocate %zu bytes", *file_size)
            read_size = fread(msg, 1, *file_size, file);
            if(read_size == *file_size) {
                return msg;
            }
            free(msg);  // Fall through to streaming if full read failed
        }
    }

    // Handle non-seekable files (pipes, special devices)
    *file_size = 0;
    msg = NULL;

    while (1) {
        uint8_t* new_buf = (uint8_t*)realloc(msg, allocated_size + chunk_size);
        RAISE_IF(new_buf == NULL, "Failed to allocate %zu bytes", allocated_size + chunk_size)
        msg = new_buf;

        read_size = fread(msg + allocated_size, 1, chunk_size, file);
        allocated_size += read_size;

        if (read_size < chunk_size) {
            if (feof(file)) {
                *file_size = allocated_size;
                return msg;
            }
            if (ferror(file)) {
                free(msg);
                RAISE("Read error after %zu bytes", allocated_size)
            }
        }
    }
}


uint8_t* read_binary_file(const char* filename, size_t* file_size, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    RAISE_IF(filename == NULL, "Found NULL filename")

    FILE* file = fopen(filename, "rb");
    RAISE_IF(file == NULL, "Failed to open file '%s'", filename)

    uint8_t* data = read_binary_fd(file, file_size, &CHILD_ERRMSG);
    fclose(file);
    RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG);

    return data;
}


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
dict_t* dict_insert(char* name, void* thing, dict_t* dict){
    dict_t* dict_new = (dict_t*)malloc(sizeof(dict_t));
    dict_new->name = strdup(name);
    dict_new->thing = thing;
    dict_new->next = dict;
    return dict_new;
}

// O(n) for this linked list implementation
void* dict_lookup(char* name, dict_t* ll){
    for(dict_t* n = ll; n != NULL; n = n->next){
        if(strcmp(n->name, name) == 0){
            return n->thing;
        }
    }
    return NULL;
}

// As currently implemented, the dictionary may have synonyms. This function
// deletes (and frees) all entries with the same name.
dict_t* dict_delete(char* name, dict_t* dict){
    dict_t* ptr;
    // remove initial values with name
    while(dict != NULL && strcmp(dict->name, name) == 0){
        ptr = dict->next;
        free(dict->name);
        free(dict);
        dict = ptr;
    }

    // remove internal/final values with name
    ptr = dict;
    while(ptr != NULL){
        if(ptr->next != NULL && strcmp(ptr->next->name, name) == 0){
            ptr->next = ptr->next->next;
            free(ptr->next->name);
            free(ptr->next);
        } else {
            ptr = ptr->next;
        }
    }
    return dict;
}

// Free the list spine AND the names, but not the elements
void dict_free(dict_t* list){
    while(list != NULL){
        dict_t* temp = list;
        list = list->next;
        free(temp->name);
        free(temp);
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
        shm_t* shm = TRY(shopen, i);
        if (shm == NULL) {
            RAISE("Shared volume %zu does not exist", ptr)
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
        shm_t* shm = TRY(shopen, i);
        if (shm == NULL) {
            RAISE("Failed to find shared volume %zu while searching for relative pointer %zu in shm at '%s'", i, ptr, shm->volume_name)
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

    RAISE_IF(shm_basename == NULL, "Undefined shm basename");

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
    TRY(get_available_memory, &available_memory);

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
        TRY(choose_next_volume_size, &new_volume_size, size);

        shm = TRY(shinit, common_basename, i, new_volume_size);

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

void* shmemcpy(void* src, size_t size, ERRMSG){
    PTR_RETURN_SETUP(void)

    void* dest = TRY(shmalloc, size);

    memmove(dest, src, size);

    return dest;
}

void* shcalloc(size_t nmemb, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    void* data = TRY(shmalloc, nmemb * size);

    memset(data, 0, nmemb * size);

    return data;
}


void* shrealloc(void* ptr, size_t size, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(size == 0, "Cannot reallocate to size 0")

    if (ptr == NULL){
        ptr = TRY(shmalloc, size);
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
        new_ptr = TRY(shmalloc, size);

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
static char schema_size_to_string(size_t size) {
  if (size < 10) {
      // characters 0-9 are integers 0-9
      return (char)(0x30 + size);
  } else if (size < 36) {
      // characters a-z are integers 10-35
      return (char)(0x61 + (size - 10));
  } else if (size < 62) {
      // characters A-Z are integers 36-61
      return (char)(0x41 + (size - 36));
  } else if (size == 62) {
      // '+' is 62
      return '+';
  } else if (size == 63) {
      // '/' is 63
      return '/';
  } else {
      // TODO: update this to encode arbitrary size
      return '\a'; // error - ding!!!
  }
}

static char* resize_schema_buffer(char* schema_str, size_t pos, size_t* buffer_size, size_t requirement){
    if(pos + requirement + 1 >= *buffer_size){
        *buffer_size += 32 * (1 + (*buffer_size - pos + requirement) / 32);
        schema_str = (char*)realloc(schema_str, *buffer_size);
    }
    return schema_str;
}

static char* write_generic_schema_r(const Schema* schema, char* schema_str, size_t* pos, size_t* buffer_size){
    schema_str = resize_schema_buffer(schema_str, *pos, buffer_size, 2);
    switch (schema->type) {
        case MORLOC_NIL:
            schema_str[*pos] = SCHEMA_NIL; (*pos)++;
            break;
        case MORLOC_BOOL:
            schema_str[*pos] = SCHEMA_BOOL; (*pos)++;
            break;
        case MORLOC_UINT8:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '1'; (*pos)++;
            break;
        case MORLOC_UINT16:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '2'; (*pos)++;
            break;
        case MORLOC_UINT32:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_UINT64:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_SINT8:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '1'; (*pos)++;
            break;
        case MORLOC_SINT16:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '2'; (*pos)++;
            break;
        case MORLOC_SINT32:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_SINT64:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_FLOAT32:
            schema_str[*pos] = SCHEMA_FLOAT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_FLOAT64:
            schema_str[*pos] = SCHEMA_FLOAT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_STRING:
            schema_str[*pos] = SCHEMA_STRING; (*pos)++;
            break;
        case MORLOC_ARRAY:
            schema_str[*pos] = SCHEMA_ARRAY; (*pos)++;
            schema_str = write_generic_schema_r(schema->parameters[0], schema_str, pos, buffer_size);
            break;
        case MORLOC_MAP:
            schema_str[*pos] = SCHEMA_MAP; (*pos)++;
            schema_str[*pos] = schema_size_to_string(schema->size); (*pos)++;
            for(size_t i = 0; i < schema->size; i++){
                // write key
                size_t key_len = strlen(schema->keys[i]);
                schema_str = resize_schema_buffer(schema_str, *pos, buffer_size, key_len + 1);
                schema_str[*pos] = schema_size_to_string(key_len); (*pos)++;
                strncpy(schema_str + *pos, schema->keys[i], key_len); *pos += key_len;
                // write value
                schema_str = write_generic_schema_r(schema->parameters[i], schema_str, pos, buffer_size);
            }
            break;

        case MORLOC_TUPLE:
            schema_str[*pos] = SCHEMA_TUPLE; (*pos)++;
            schema_str[*pos] = schema_size_to_string(schema->size); (*pos)++;
            for(size_t i = 0; i < schema->size; i++){
                schema_str = write_generic_schema_r(schema->parameters[i], schema_str, pos, buffer_size);
            }
            break;
        default:
            // This should be unreachable
            fprintf(stderr, "Missing case in morloc schema");
    }

    return schema_str;
}

char* schema_to_string(const Schema* schema){
    char* schema_str = (char*)calloc(32, sizeof(char));
    size_t pos = 0;
    size_t buffer_size = 32;
    schema_str = write_generic_schema_r(schema, schema_str, &pos, &buffer_size);
    schema_str[pos] = '\0';
    return schema_str;
}

// Allocate a shared memory block sufficient to store a schema's object
void* get_ptr(const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(void)
    void* ptr = TRY((void*)shmalloc, schema->width);
    return ptr;
}

// The voidstar representation of variable length data
typedef struct Array {
  size_t size;
  relptr_t data;
} Array;


// Check is the datastructure defined by a schema has fixed length.
//
// This will be true if there are no arrays in the structure.
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


static size_t calculate_voidstar_size(const void* data, const Schema* schema, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0)

    size_t size = 0;

    switch(schema->type){
        case MORLOC_STRING:
            {
                Array* array = (Array*)data;
                size = sizeof(Array) + array->size * schema->parameters[0]->width;
            }
            break;
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                size_t element_width = schema->parameters[0]->width;

                uint8_t* element_data = TRY((uint8_t*)rel2abs, array->data);

                size = sizeof(Array);

                if (schema_is_fixed_width(schema)){
                    size += element_width * array->size;
                } else {
                    for(size_t i = 0; i < array->size; i++){
                        size += TRY(calculate_voidstar_size, element_data + i * element_width, schema->parameters[0]);
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                if (schema_is_fixed_width(schema)){
                    size = schema->width;
                } else {
                    uint8_t* element_data = (uint8_t*)data;
                    for(size_t i = 0; i < schema->size; i++){
                        size += TRY(calculate_voidstar_size, element_data + schema->offsets[i], schema->parameters[i]);
                    }
                }
            }
            break;
        default:
            size = schema->width;
            break;
    }
    return size;
}


// Prototypes

Schema* parse_schema(const char** schema_ptr, ERRMSG);

// Helper function to create a schema with parameters
static Schema* create_schema_with_params(morloc_serial_type type, size_t width, size_t size, Schema** params, char** keys) {
    Schema* schema = (Schema*)calloc(1, sizeof(Schema));
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
        fprintf(stderr, "Floats may only have widths of 4 or 8 bytes, found %lu", width);
        return NULL;
    }
}

static Schema* string_schema() {
    Schema** params = (Schema**)calloc(1, sizeof(Schema*));
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
    Schema** params = (Schema**)calloc(1, sizeof(Schema*));
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
  hint = (char*)calloc(1, buffer_size);
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
      RAISE("Unrecognized schema type '%c'", c);
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

    *packet = (char*)calloc(BUFFER_SIZE, sizeof(char));
    RAISE_IF(*packet == NULL, "\n%s", "Empty packet")

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
    // parse the msgpack tuple
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
        RAISE("Bad token %d", token->type);
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
        RAISE("Failed to parse morloc type %d", schema->type)
    }
    return EXIT_PASS;
}

int unpack_with_schema(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr, ERRMSG) {
    INT_RETURN_SETUP

    // Pass once over the MessagePack data, calculating the allocation size
    size_t size = msg_size(mgk, mgk_size, schema);

    void* mlc = TRY((void*)shmalloc, size);

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

// {{{ morloc JSON support

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

    char* output = (char*)calloc(output_len + 1, sizeof(char)); // +1 for null terminator
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

static void consume_whitespace(char** json_data){
    while(**json_data == ' ' || **json_data == '\t' || **json_data == '\n' || **json_data == '\r'){
        (*json_data)++;
    }
}

static bool consume_char(char c, char** json_ptr, ERRMSG){
    BOOL_RETURN_SETUP
    consume_whitespace(json_ptr);
    RAISE_IF(**json_ptr != c, "\n  Unexpected character: expected '%c' at '%s'", c, *json_ptr)
    (*json_ptr)++;
    return true;
}

static void consume_many(char* cs, char** json_ptr){
    bool match = true;
    while(match){
        match = false;
        for(size_t i = 0; cs[i] != '\0'; i++){
            if((**json_ptr) == cs[i]){
                match = true;
                (*json_ptr)++;
                break;
            }
        }
    }
}

static bool match_json_boolean(char** json_data, ERRMSG){
    BOOL_RETURN_SETUP
    consume_whitespace(json_data);
    if(**json_data == 't' && *(*json_data + 1) == 'r' && *(*json_data + 2) == 'u' && *(*json_data + 3) == 'e') {
        *json_data += 4;
        return true;
    } else if(**json_data == 'f' && *(*json_data + 1) == 'a' && *(*json_data + 2) == 'l' && *(*json_data + 3) == 's' && *(*json_data + 4) == 'e') {
        *json_data += 5;
        return false;
    } else {
        RAISE("\n  Expected (true/false) value in JSON")
    }
}

static int64_t parse_json_int(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(uint64_t, 0)
    consume_whitespace(json_ptr);
    char* end;
    int64_t val = strtoll(*json_ptr, &end, 10);
    RAISE_IF(*json_ptr == end, "\n  Not a valid integer: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

static uint64_t parse_json_uint(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(uint64_t, 0)
    consume_whitespace(json_ptr);
    char* end;
    uint64_t val = strtoull(*json_ptr, &end, 10);
    RAISE_IF(*json_ptr == end, "\n  Not a valid integer: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

static double parse_json_double(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(double, 0)
    consume_whitespace(json_ptr);
    char* end;
    double val = strtod(*json_ptr, &end);
    RAISE_IF(*json_ptr == end, "\n  Not a valid floating-point number: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

// Calculate the number of characters in a JSON string
//
// This count is equal to the number of characters in the C string. Escaped
// special characters count as one. Any formatting errors will be caught.
static int json_string_size(char* ptr, size_t* json_size, size_t* c_size, ERRMSG) {
    INT_RETURN_SETUP
    RAISE_IF(*ptr != '"', "\n  Expected string, but no initial quote found (observed '%c')", *ptr)

    char* json_start = ptr;
    size_t c_size_ = 0;

    ptr++; // Skip opening quote
    while (*ptr != '\0' && *ptr != '"') {
        if (*ptr == '\\') {
            ptr++; // Move past backslash
            RAISE_IF(*ptr == '\0', "\n  Unexpected end of string in escape sequence");

            switch (*ptr) {
                case '"':
                case '\\':
                case '/':
                case 'b':
                case 'f':
                case 'n':
                case 'r':
                case 't':
                    // Valid single-character escapes
                    c_size_ += 1;
                    ptr++;
                    break;

                case 'u': {
                    // Unicode escape sequence \uXXXX
                    ptr++; // Move past 'u'
                    // Validate 4 hex digits
                    for (int i = 0; i < 4; i++) {
                        RAISE_IF(*ptr == '\0', "\n  Truncated Unicode escape");
                        RAISE_IF(!isxdigit(*ptr), "\n  Invalid hex digit in Unicode escape");
                        ptr++;
                    }
                    c_size_ += 4; // Count as one character
                    break;
                }

                default:
                    RAISE("\n  Invalid escape character");
            }
        } else {
            // Regular character
            c_size_ += 1;
            ptr++;
        }
    }

    RAISE_IF(*ptr != '"', "\n  Unterminated string, missing closing quote");
    ptr++;

    *json_size = (size_t)(ptr - json_start - 2);
    *c_size = c_size_;

    return EXIT_PASS;
}

static int write_json_string(char** json_ptr, char* dest, ERRMSG){
    INT_RETURN_SETUP
    consume_whitespace(json_ptr);

    char* ptr = *json_ptr;

    RAISE_IF(*ptr != '"', "\n  Expected string, but no initial quote found")
    ptr++; // Skip opening quote

    while (*ptr != '\0' && *ptr != '"') {
        if (*ptr == '\\') {
            ptr++; // Move past backslash
            RAISE_IF(*ptr == '\0', "\n  Unexpected end of string in escape sequence");

            switch (*ptr) {
                case '"':
                    *dest = '"'; dest++; break;
                case '\\':
                    *dest = '\\'; dest++; break;
                case '/':
                    *dest = '/'; dest++; break;
                case 'b':
                    *dest = '\b'; dest++; break;
                case 'f':
                    *dest = '\f'; dest++; break;
                case 'n':
                    *dest = '\n'; dest++; break;
                case 'r':
                    *dest = '\r'; dest++; break;
                case 't':
                    *dest = '\t'; dest++; break;
                case 'u': {
                    // Unicode escape sequence \uXXXX
                    ptr++; // Move past 'u'
                    // Validate 4 hex digits
                    for (int i = 0; i < 4; i++) {
                        RAISE_IF(*ptr == '\0', "\n  Truncated Unicode escape");
                        RAISE_IF(!isxdigit(*ptr), "\n  Invalid hex digit in Unicode escape");
                        *dest = *ptr;
                        dest++;
                        ptr++;
                    }
                    break;
                }
                default:
                    RAISE("\n  Invalid escape character");
            }
            ptr++;
        } else {
            // Regular character
            *dest = *ptr;
            ptr++;
            dest++;
        }
    }

    RAISE_IF(*ptr != '"', "\n  Unterminated string, missing closing quote");
    ptr++; // move past closing quote

    // consume the string
    *json_ptr = ptr;

    return EXIT_PASS;

}

static char* read_json_key(char** json_ptr, ERRMSG){
    PTR_RETURN_SETUP(char)

    size_t j_string_size = 0;
    size_t c_string_size = 0;

    consume_whitespace(json_ptr);
    TRY(json_string_size, *json_ptr, &j_string_size, &c_string_size);

    char* key = (char*)calloc(c_string_size + 1, sizeof(char));

    // If the C and JSON strings are the same length, then there are
    // no special characters to consider and we can simply memcpy.
    if(c_string_size == j_string_size){
        // copy from the position AFTER the initial quote
        memcpy(key, *json_ptr + 1, c_string_size);
        // consume the string and quotes
        *json_ptr += j_string_size + 2;
    }
    // Otherwise we need to loop through the string and handle escapees
    else {
        TRY(write_json_string, json_ptr, key);
    }

    return key;
}

// loop to the end of an array, counting the elements
static size_t json_array_size(char* ptr, ERRMSG) {
    VAL_RETURN_SETUP(size_t, 0)

    RAISE_IF(*ptr != '[', "\n  Failed to parse input JSON. Expected an array, but found no starting bracket\n");

    size_t size = 0;
    size_t depth = 0;
    bool in_string = false;
    bool in_escape = false;

    while(*ptr != '\0'){
        if(in_string){
            switch(*ptr){
                case '\\':
                    in_escape = !in_escape;
                    break;
                case '"':
                    if(!in_escape){
                        in_string = false;
                    }
                // fall through
                default:
                    in_escape = false;
                    break;
            }
        } else {
            switch(*ptr){
                case '[': {
                  depth++;
                  // handle the first element
                  if(depth == 1){
                    consume_whitespace(&ptr);
                    if(*(ptr+1) != ']'){
                        size = 1;
                    }
                  }
                  break;
                }
                case '{': depth++; break;
                case ']': depth--; break;
                case '}': depth--; break;
                case '"':
                    in_string = true;
                    in_escape = false;
                    break;
                case ',':
                    size += depth == 1;
                    break;
                default:
                    break;
            }
        }

        if(depth == 0){
            return size;
        }

        ptr++;
    }

    RAISE("\n  Failed to parse JSON: missing closing bracket on array\n")
}

// input JSON data should be NULL terminated
int read_json_with_schema_r(
    uint8_t* voidstar, // pre-allocated and zeroed space that will be mutated
    char** json_ptr, // a pointer to the current location in the json string
    const Schema* schema,
    ERRMSG
){

    INT_RETURN_SETUP
    consume_whitespace(json_ptr);

    switch(schema->type){
        case MORLOC_NIL: {
            *((uint8_t*)voidstar) = (uint8_t)0;
            break;
        }
        case MORLOC_BOOL: {
            bool val = TRY(match_json_boolean, json_ptr);
            *((uint8_t*)voidstar) = val ? (uint8_t)1  // true
                                        : (uint8_t)0; // false
            break;
        }

        case MORLOC_SINT8: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT8_MIN || val > INT8_MAX, "Value out of range for 8-bit signed integer");
            *((int8_t*)voidstar) = (int8_t)val;
            break;
        }

        case MORLOC_SINT16: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT16_MIN || val > INT16_MAX, "Value out of range for 16-bit signed integer");
            *((int16_t*)voidstar) = (int16_t)val;
            break;
        }

        case MORLOC_SINT32: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT32_MIN || val > INT32_MAX, "Value out of range for 32-bit signed integer");
            *((int32_t*)voidstar) = (int32_t)val;
            break;
        }

        case MORLOC_SINT64: {
            int64_t val = TRY(parse_json_int, json_ptr);
            *((int64_t*)voidstar) = val;
            break;
        }

        case MORLOC_UINT8: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT8_MAX, "Value out of range for 8-bit unsigned integer");
            *((uint8_t*)voidstar) = (uint8_t)val;
            break;
        }

        case MORLOC_UINT16: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT16_MAX, "Value out of range for 16-bit unsigned integer");
            *((uint16_t*)voidstar) = (uint16_t)val;
            break;
        }

        case MORLOC_UINT32: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT32_MAX, "Value out of range for 32-bit unsigned integer");
            *((uint32_t*)voidstar) = (uint32_t)val;
            break;
        }

        case MORLOC_UINT64: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            *((uint64_t*)voidstar) = val;
            break;
        }

        case MORLOC_FLOAT32: {
            double val = TRY(parse_json_double, json_ptr);
            RAISE_IF((val < -FLT_MAX || val > FLT_MAX) && !isinf(val), "Value out of range for 32-bit float");
            *((float*)voidstar) = (float)val;
            break;
        }

        case MORLOC_FLOAT64: {
            double val = TRY(parse_json_double, json_ptr);
            *((double*)voidstar) = val;
            break;
        }

        case MORLOC_STRING: {
            size_t j_string_size = 0;
            size_t c_string_size = 0;

            Array* arr = (Array*)voidstar;
            arr->size = 0;
            arr->data = RELNULL;

            TRY(json_string_size, *json_ptr, &j_string_size, &c_string_size);

            if(c_string_size == 0){
                // eat the empty string and leave
                TRY(consume_char, '"', json_ptr);
                TRY(consume_char, '"', json_ptr);
                break;
            }

            absptr_t mlc_str = TRY(shcalloc, c_string_size, sizeof(char));

            // If the C and JSON strings are the same length, then there are
            // no special characters to consider and we can simply memcpy.
            if(c_string_size == j_string_size){
                // copy from the position AFTER the initial quote
                memcpy(mlc_str, (*json_ptr) + 1, c_string_size);
                // consume the string and quotes
                *json_ptr += j_string_size + 2;
            }
            // Otherwise we need to loop through the string and handle escapees
            else {
                TRY(write_json_string, json_ptr, (char*)mlc_str);
            }

            arr->size = c_string_size;
            arr->data = TRY(abs2rel, mlc_str)

            break;
        }

        case MORLOC_ARRAY: {
            size_t size = TRY(json_array_size, *json_ptr);

            Array* arr = (Array*)voidstar;
            arr->size = size;

            TRY(consume_char, '[', json_ptr);

            if(size == 0){
                arr->data = RELNULL; // handle empty array
            } else {
                absptr_t array_data = TRY(shcalloc, size, schema->parameters[0]->width);

                for(size_t element_idx = 0; element_idx < size; element_idx++){
                    uint8_t* element = (uint8_t*)array_data + element_idx * schema->parameters[0]->width;
                    TRY(read_json_with_schema_r, element, json_ptr, schema->parameters[0])
                    consume_whitespace(json_ptr);
                    if(element_idx < size - 1){
                        TRY(consume_char, ',', json_ptr);
                    }
                }
                arr->data = TRY(abs2rel, array_data);
            }

            TRY(consume_char, ']', json_ptr);

            break;
        }

        case MORLOC_TUPLE: {
            TRY(consume_char, '[', json_ptr);

            for(size_t element_idx = 0; element_idx < schema->size; element_idx++){
                uint8_t* element = voidstar + schema->offsets[element_idx];
                TRY(read_json_with_schema_r, element, json_ptr, schema->parameters[element_idx])
                if(element_idx < (schema->size - 1)){
                    TRY(consume_char, ',', json_ptr);
                }
            }

            TRY(consume_char, ']', json_ptr);

            break;
        }

        case MORLOC_MAP: {
            TRY(consume_char, '{', json_ptr);

            uint8_t* element_ptr = NULL;
            size_t nentries_complete = 0;
            while(**json_ptr != '}'){
                RAISE_IF(**json_ptr == '\0', "Unexpected end of JSON object")

                // match '<key><space>:<space>"
                consume_whitespace(json_ptr);
                char* key = TRY(read_json_key, json_ptr)
                TRY(consume_char, ':', json_ptr);

                // Search for key in schema and write to appropriate offset
                bool parse_failed = true;
                for(size_t element_idx = 0; element_idx < schema->size; element_idx++){
                    // if the key is matches, parse to the appropriate offset
                    if(strcmp(key, schema->keys[element_idx]) == 0){
                        FREE(key)
                        element_ptr = voidstar + schema->offsets[element_idx];
                        TRY(
                            read_json_with_schema_r,
                            element_ptr,
                            json_ptr,
                            schema->parameters[element_idx]
                        )
                        nentries_complete++;
                        parse_failed = false;
                        break;
                    }
                }
                RAISE_IF_WITH(parse_failed, free(key), "Could not find key %s in record", key)
                FREE(key);

                consume_whitespace(json_ptr);
                if(**json_ptr == ','){
                    (*json_ptr)++;
                }
            }
            RAISE_IF(nentries_complete != schema->size, "Expected %zu entries in JSON object, only found %zu", schema->size, nentries_complete)

            TRY(consume_char, '}', json_ptr);
            break;
        }

        default:
            RAISE("Unhandled schema type %c", **json_ptr)
            break;
    }

    return EXIT_PASS;
}

// Returns a pointer to voidstar data
relptr_t read_json_with_schema(char* json_data, const Schema* schema, ERRMSG){
    VAL_RETURN_SETUP(relptr_t, RELNULL)

    char* json_ptr = json_data;

    uint8_t* voidstar = (uint8_t*)TRY(shcalloc, 1, schema->width);
    TRY(read_json_with_schema_r, voidstar, &json_ptr, schema);

    relptr_t ptr = TRY(abs2rel, (absptr_t)voidstar);

    return ptr;
}

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

                if(array->size == 0){
                    printf("\"\"");
                    break;
                }

                data = TRY((const char*)rel2abs, array->data);

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

                if(array->size == 0){
                    printf("[]");
                    break;
                }

                data = TRY((const char*)rel2abs, array->data);

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
    // IMPORTANT: the newline distinguishes JSON from MessagePack for 0-9 values
    printf("\n");

    return success;
}


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
static char* find_end_of_element(char* json, ERRMSG){
    PTR_RETURN_SETUP(char)

    size_t size = 0;

    switch(*json){
        case '"':
            // handle string, size is the number of characters until the closing quote
            size_t json_size;
            size_t c_size;
            TRY(json_string_size, json, &json_size, &c_size);
            json += json_size + 2;
            break;
        case '[':
            // handle array, size is the number of characters until the closing bracket
            size = TRY(json_array_size, json)
            json += size + 1;
            break;
        case '{':
            // handle object, size is the number of characters until the closing brace
            TRY(consume_char, '{', &json)
            consume_whitespace(&json);
            while(*json != '}'){
                // parse key
                json = TRY(find_end_of_element, json);
                // parse entry separator
                TRY(consume_char, ':', &json)
                // parse object
                json = TRY(find_end_of_element, json);
                // parse object separater if present
                consume_whitespace(&json);
                if(*json == ','){
                    TRY(consume_char, ',', &json)
                }
            }
            TRY(consume_char, '}', &json)
            break;
        case 't':
            // handle boolean, size is 4, if the "true" is matched
            if(strncmp(json, "true", 4) == 0){
                return json + 4;
            } else {
                RAISE("Invalid JSON")
            }
            break;
        case 'f':
            // handle boolean, size is 5, if the "false" is matched
            if(strncmp(json, "false", 5) == 0){
                return json + 5;
            } else {
                RAISE("Invalid JSON")
            }
            break;
        case 'n':
            // handle boolean, size is 4, if the "null" is matched
            if(strncmp(json, "null", 4) == 0){
                return json + 4;
            } else {
                RAISE("Invalid JSON")
            }
            break;
        case '-':
        case '+':
        case '.':
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            {
                // handle integer or real number
                char decimal_chars[] = "0123456789+-.eE";
                consume_many(decimal_chars, &json);
            }
            break;
        default:
            RAISE("Invalid JSON element: %s\n", json)
    }

    return json;
}

char* access_json_by_path(char* json, path_t* path, size_t path_length, ERRMSG){
    PTR_RETURN_SETUP(char)

    if(json == NULL){
        RAISE("json must not be null")
    }

    // base case
    if(path == NULL || path_length == 0){
        char* json_end = TRY(find_end_of_element, json);
        return strndup(json, (size_t)(json_end - json));
    }

    if(path->json_path_type == JSON_PATH_TYPE_KEY){

        char* key = path->element.key;

        // The JSON data should be an object
        TRY(consume_char, '{', &json);

        // iterate through the object until the required key is found
        while(*json != '\0'){
            char* json_key = TRY(read_json_key, &json);

            TRY(consume_char, ':', &json);
            consume_whitespace(&json);

            // the desired object was found, so recurse into it and return the result
            if(strcmp(key, json_key) == 0){
                free(json_key);
                break;
            } else {
                // if the key did not match, move past the value to the next key
                json = TRY(find_end_of_element, json);
                consume_whitespace(&json);
                if((*json) == ','){
                    TRY(consume_char, ',', &json);
                }
            }
            free(json_key);
        }

        consume_whitespace(&json);
        if (*json == '}') {
            RAISE("Key not found");
        }

    } else if((*path).json_path_type == JSON_PATH_TYPE_IDX) {
        // The JSON data should be an array, iterate through it until the
        // required index is found. Then return the value.
        TRY(consume_char, '[', &json);

        size_t idx = path->element.index;

        // iterate through the object until the required key is found
        for(size_t i = 0; i < idx; i++){
            consume_whitespace(&json);
            json = TRY(find_end_of_element, json);
            consume_whitespace(&json);
            if(*json == ']'){
                if(i != idx){
                    RAISE("Invalid JSON input: cannot extract element %zu since has only %zu elements", idx, i+1)
                }
                break;
            } else {
                TRY(consume_char, ',', &json);
            }
        }

    } else {
        // This should be unreachable
        RAISE("Unexpected json_path_type\n")
    }

    // if there are more elements in the path, recurse
    char* result = TRY(access_json_by_path, json, path + 1, path_length - 1);

    return result;
}

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

typedef struct morloc_metadata_header_s {
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


morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG){
    PTR_RETURN_SETUP(morloc_packet_header_t)

    RAISE_IF(msg == NULL, "Cannot make packet from NULL pointer")

    morloc_packet_header_t* header = (morloc_packet_header_t*) msg;
    RAISE_IF(header->magic != MORLOC_PACKET_MAGIC, "Malformed morloc packet")

    return header;
}


bool packet_is_ping(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_PING;
}


bool packet_is_local_call(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_CALL &&
           header->command.call.entrypoint == PACKET_ENTRYPOINT_LOCAL;
}

bool packet_is_remote_call(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_CALL &&
           header->command.call.entrypoint == PACKET_ENTRYPOINT_REMOTE_SFS;
}


size_t morloc_packet_size_from_header(const morloc_packet_header_t* header){
    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}


size_t morloc_packet_size(const uint8_t* packet, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0);

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}


// TODO: what should our submarine reflect?
// Currently I just return an exact copy of the ping packet
uint8_t* return_ping(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    TRY(packet_is_ping, packet);

    size_t size = TRY(morloc_packet_size, packet);

    uint8_t* pong = (uint8_t*)calloc(size, sizeof(uint8_t));
    memcpy(pong, packet, size);

    return pong;
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


static void set_morloc_metadata_header(uint8_t* metadata, uint8_t metadata_type, uint32_t metadata_length){
    morloc_metadata_header_t* metadata_header = (morloc_metadata_header_t*)metadata;
    metadata_header->magic[0] = 'm';
    metadata_header->magic[1] = 'm';
    metadata_header->magic[2] = 'h';
    metadata_header->type = metadata_type;
    metadata_header->size = (uint32_t)metadata_length;
}


uint8_t* make_ping_packet(){
    uint8_t* packet = (uint8_t*)calloc(1, sizeof(morloc_packet_header_t));

    packet_command_t cmd = {
        .ping = {
            PACKET_TYPE_PING,
            .padding = { 0, 0, 0, 0, 0, 0, 0 }
        }
    };

    set_morloc_packet_header(packet, cmd, 0, 0);

    return packet;
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
  uint8_t* packet = (uint8_t*)calloc(packet_length, sizeof(uint8_t));

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


static uint8_t* make_morloc_data_packet_with_schema(
    const uint8_t* data,
    size_t data_length,
    const Schema* schema,
    uint8_t src,
    uint8_t fmt,
    uint8_t cmpr,
    uint8_t encr,
    uint8_t status
){
  if(schema == NULL){
    return make_morloc_data_packet(data, data_length, NULL, 0, src, fmt, cmpr, encr, status);
  }

  char* schema_str = schema_to_string(schema);
  size_t metadata_length = strlen(schema_str) + 1; // +1 for null byte
  size_t metadata_length_total = (1 + (sizeof(morloc_metadata_header_t) + metadata_length) / 32) * 32;
  uint8_t* metadata = (uint8_t*)calloc(metadata_length_total, sizeof(char));

  set_morloc_metadata_header(metadata, MORLOC_METADATA_TYPE_SCHEMA_STRING, metadata_length);
  memcpy(metadata + sizeof(morloc_metadata_header_t), schema_str, metadata_length);
  FREE(schema_str);

  return make_morloc_data_packet(data, data_length, metadata, metadata_length_total, src, fmt, cmpr, encr, status);
}


// Make a data packet from a relative pointer to shared memory data
// Include the schema in the packet metadata
uint8_t* make_standard_data_packet(relptr_t ptr, const Schema* schema){
  uint8_t* packet = make_morloc_data_packet_with_schema(
        NULL, sizeof(relptr_t),
        schema,
        PACKET_SOURCE_RPTR,
        PACKET_FORMAT_VOIDSTAR,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS
    );

  morloc_packet_header_t* header = (morloc_packet_header_t*)(packet);

  *((ssize_t*)(packet + sizeof(morloc_packet_header_t) + (size_t)header->offset)) = ptr;

  return packet;
}


uint8_t* make_mpk_data_packet(const char* mpk_filename, const Schema* schema){
    uint8_t* packet = make_morloc_data_packet_with_schema(
        (const uint8_t*)mpk_filename,
        strlen(mpk_filename),
        schema,
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS
    );
    return packet;
}


// Returns a metadata header if the magic matches, otherwise returns NULL
morloc_metadata_header_t* as_morloc_metadata_header(const uint8_t* ptr){

    morloc_metadata_header_t* metadata_header = (morloc_metadata_header_t*)(ptr);

    if(strncmp(metadata_header->magic, MORLOC_METADATA_HEADER_MAGIC, 3) != 0){
        return NULL;
    }

    return metadata_header;
}


// Read a data packet type schema from the packet metadata
// If no type schema is defined, return NULL
// Raise an error if the packet is malformed
// On success, return a pointer to the schema string in the packet (no copying)
char* read_schema_from_packet_meta(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(char)

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    if(header->offset >= sizeof(morloc_metadata_header_t)){
        size_t offset = 0;
        do {
            morloc_metadata_header_t* metadata_header = as_morloc_metadata_header(packet + sizeof(morloc_packet_header_t));
            if(metadata_header == NULL){
                return NULL;
            }
            offset += sizeof(morloc_metadata_header_t);
            if(metadata_header != NULL && metadata_header->type == MORLOC_METADATA_TYPE_SCHEMA_STRING) {
                char* schema_str = (char*)packet + sizeof(morloc_packet_header_t) + offset;
                return schema_str;
            } else {
                offset += metadata_header->size;
            }
        } while(offset <= (header->offset - sizeof(morloc_metadata_header_t)));
    }

    return NULL;
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


// Returns the error message if this packet failed and NULL otherwise
char* get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG){
    PTR_RETURN_SETUP(char)

    char* packet_err = NULL;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, data);

    if (header->command.data.status == PACKET_STATUS_FAIL) {
        packet_err = (char*)calloc(header->length + 1, sizeof(char));
        RAISE_IF(packet_err == NULL, "Failed to allocate error message");

        char* packet_data = (char*)data + sizeof(morloc_packet_header_t) + (size_t)header->offset;
        void* copy_ptr = memcpy(packet_err, packet_data, header->length);
        RAISE_IF(copy_ptr == NULL, "Failed to copy error message to packet")

        return packet_err;
    }

    return packet_err;
}


// Opens a data packet returning a pointer to a packet
//  * If packet has failing status, it is returned unchanged (error propagation)
//  * If packet handling fails, the fail_packet is defined and NULL is returned
//  * Else an absolute pointer to voidstar data is returned
uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    uint8_t source;
    uint8_t format;

    void* voidstar = NULL;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, data);

    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected a data packet");

    source = header->command.data.source;
    format = header->command.data.format;

    char* packet_error = TRY(get_morloc_data_packet_error_message, data);
    RAISE_IF(packet_error != NULL, "\n%s", packet_error)

    switch (source) {
        case PACKET_SOURCE_MESG:
            if (format == PACKET_FORMAT_MSGPACK) {
                TRY(
                    unpack_with_schema,
                    (const char*)data + sizeof(morloc_packet_header_t) + header->offset,
                    header->length,
                    schema,
                    &voidstar
                );
            } else {
                RAISE("Invalid format from mesg: 0x%02hhx", format);
            }
            break;
        case PACKET_SOURCE_FILE:
            switch (format) {
                case PACKET_FORMAT_MSGPACK: {
                    char* filename = strndup((char*)data + sizeof(morloc_packet_header_t) + header->offset, MAX_FILENAME_SIZE);
                    size_t file_size;
                    uint8_t* msg = TRY(read_binary_file, filename, &file_size);
                    FREE(filename);
                    // Unpack the binary buffer using the schema
                    TRY(unpack_with_schema, (const char*)msg, file_size, schema, &voidstar);
                    FREE(msg);
                    break;
                }
                default:
                    RAISE("Invalid format from file: 0x%02hhx", format);
                    return NULL;
            }
            break;

        case PACKET_SOURCE_RPTR:
            if (format == PACKET_FORMAT_VOIDSTAR) {
                // This packet should contain a relative pointer as its payload
                size_t relptr = *(size_t*)(data + header->offset + sizeof(morloc_packet_header_t));
                voidstar = TRY(rel2abs, relptr);
            } else {
                RAISE("For RPTR source, expected voidstar format, found: 0x%02hhx", format);
                return NULL;
            }
            break;

        default:
            RAISE("Invalid source");
    }

    return (uint8_t*)voidstar;
}


static uint8_t* make_morloc_call_packet_gen(uint32_t midx, uint8_t entrypoint, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    size_t data_length = 0;
    uint32_t offset = 0;

    for(size_t i = 0; i < nargs; i++){
        morloc_packet_header_t* arg = TRY(read_morloc_packet_header, arg_packets[i]);

        data_length += sizeof(morloc_packet_header_t) + (size_t)arg->offset + (size_t)arg->length;
    }

    size_t packet_length = data_length + offset + sizeof(morloc_packet_header_t);

    uint8_t* data = (uint8_t*)calloc(packet_length, sizeof(uint8_t));

    packet_command_t cmd = {
      .call = {
        .type = PACKET_TYPE_CALL,
        .entrypoint = entrypoint,
        .padding = {0, 0},
        .midx = midx,
      }
    };

    set_morloc_packet_header(data, cmd, offset, data_length);

    size_t arg_start = sizeof(morloc_packet_header_t) + offset;
    for(size_t i = 0; i < nargs; i++){
      morloc_packet_header_t* arg = TRY(read_morloc_packet_header, arg_packets[i]);
      size_t arg_length = morloc_packet_size_from_header(arg);
      memcpy(data + arg_start, arg, arg_length);
      arg_start += arg_length;
    }

    return data;
}


// Make a packet wrapping a call on the local machine
uint8_t* make_morloc_local_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(make_morloc_call_packet_gen, midx, PACKET_ENTRYPOINT_LOCAL, arg_packets, nargs);
    return packet;
}


// Make a packet wrapping a call on a remote worker
uint8_t* make_morloc_remote_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(make_morloc_call_packet_gen, midx, PACKET_ENTRYPOINT_REMOTE_SFS, arg_packets, nargs);
    return packet;
}


morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(morloc_call_t)

    morloc_call_t* call = (morloc_call_t*)calloc(1, sizeof(morloc_call_t));
    RAISE_IF(call == NULL, "calloc failed: %s", strerror(errno))

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_CALL, "Expected packet to be a call")

    call->midx = header->command.call.midx;
    call->nargs = 0;
    call->args = NULL;

    size_t start_pos = sizeof(morloc_packet_header_t) + header->offset;
    size_t end_pos = start_pos + header->length;
    size_t pos = start_pos;
    while (pos < end_pos) {
        pos += TRY(morloc_packet_size, packet + pos);
        call->nargs++;
    }

    call->args = (uint8_t**)calloc(call->nargs, sizeof(uint8_t*));
    pos = sizeof(morloc_packet_header_t) + header->offset;
    for(size_t i = 0; i < call->nargs; i++){
        morloc_packet_header_t* arg_header = read_morloc_packet_header(packet + pos, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read call argument #%zu:\n%s", i, CHILD_ERRMSG)
        RAISE_IF(
            arg_header->command.cmd_type.type != PACKET_TYPE_DATA,
            "Argument #%zu is not a DATA packet (type=%d)",
            i,
            arg_header->command.cmd_type.type
        );

        size_t arg_packet_size = morloc_packet_size_from_header(arg_header);

        // Copy the argument
        // We alternatively we could avoid this copy and pass the pointer to the
        // original memory. This would improve performance, but would be less
        // safe. I'll leave this optimization for later.
        call->args[i] = (uint8_t*)calloc(arg_packet_size, sizeof(uint8_t));
        memcpy(call->args[i], packet + pos, arg_packet_size);

        pos += arg_packet_size;
    }

    return call;
}


// }}} end packet support

// {{{ morloc packet IO

// * recurse through the data structure
// * for each array, calculate the full size of the data it points to (subtract
//   the 16 byte Array struct), add this to data_index
static relptr_t print_voidstar_binary_binder_r(
    const void* data,
    const Schema* schema,
    relptr_t data_index,
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    switch (schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                // copy the array struct
                Array array = *(Array*)data;
                // set the element pointer to the current data location
                array.data = data_index;
                // print just the array struct, the data will be printed later
                TRY(print_binary, (char*)(&array), sizeof(Array));
                // reserve space for the array and all its recursive contents
                size_t data_size = TRY(calculate_voidstar_size, data, schema);
                data_index += data_size - sizeof(Array);
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                // recursively print every tuple element
                for(size_t i = 0; i < schema->size; i++){
                    void* child = (void*)((char*)data + schema->offsets[i]);
                    data_index = TRY(print_voidstar_binary_binder_r, child, schema->parameters[i], data_index);
                }
            }
            break;
        default:
            {
                // print primitives
                TRY(print_binary, (char*)data, schema->width);
            }
    }
    return data_index;
}

// forward declaration, the *data_r and *arra_r functions are mutually recursive
static relptr_t print_voidstar_binary_array_r(const void*, const Schema*, size_t, relptr_t, ERRMSG);

// * recurse through the data structure
// * call print_voidstar_binary_array_r on each array
static relptr_t print_voidstar_binary_data_r(
    const void* data,
    const Schema* schema,
    relptr_t data_index, // pointer to start of the data for all arrays
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    switch (schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                void* absptr = TRY(rel2abs, array->data);
                data_index = TRY(print_voidstar_binary_array_r, absptr, schema->parameters[0], array->size, data_index);
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                // recursively print every tuple element
                for(size_t i = 0; i < schema->size; i++){
                    void* child = (void*)((char*)data + schema->offsets[i]);
                    data_index = TRY(print_voidstar_binary_data_r, child, schema->parameters[i], data_index);
                }
            }
            break;
        default:
            break;
    }
    return data_index;
}

// * map print_voidstar_binary_binder_r over elements
// * map print_voidstar_binary_data_r over elements
static relptr_t print_voidstar_binary_array_r(
    const void* data, // pointer to the start of the array
    const Schema* schema, // schema for the element type
    size_t length, // number of elements in the array
    relptr_t data_index, // relative pointer to the start of the data block
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    // all the fixed-size elements will be printed first
    size_t fixed_array_size = schema->width * length;
    data_index += fixed_array_size;
    if (schema_is_fixed_width(schema)){
        TRY(print_binary, (char*)data, fixed_array_size);
    } else {
        // print fixed width array elements
        relptr_t binder_ptr = data_index; // for array pointers, don't reuse
        for(size_t i = 0; i < length; i++){
            void* child = (void*)((char*)data + i * schema->width);
            binder_ptr = TRY(print_voidstar_binary_binder_r, child, schema, binder_ptr);
        }
        // print array data
        for(size_t i = 0; i < length; i++){
            void* child = (void*)((char*)data + i * schema->width);
            data_index = TRY(print_voidstar_binary_data_r, child, schema, data_index);
        }
    }
    return data_index;
}

// Print voidstar data relative to 0
//
// All array relative pointers need to be readjusted, since we are printing, all
// actions have to be linear. The existing structure is not changed and no new
// memory is allocated.
//
// On success, returns a relptr to the position after the data (probably not
// useful unless you want to calculate the size of the printed data)
static relptr_t print_voidstar_binary(
    const void* data,
    const Schema* schema,
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)
    relptr_t data_index = (relptr_t)schema->width;
    TRY(print_voidstar_binary_binder_r, data, schema, data_index);
    data_index = TRY(print_voidstar_binary_data_r, data, schema, data_index);
    return data_index;
}


// Print morloc data packet to a file descriptor
//  * If the packet has the failed bit set, an error will be raised
//  * If the data is stored as a file or message, the exact packet is printed
//  * If the packet contains a relative pointer, it will be linearized and included as raw data
int print_morloc_data_packet(const uint8_t* packet, const Schema* schema, ERRMSG){
    INT_RETURN_SETUP

    uint8_t source;
    uint8_t format;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    size_t packet_size = morloc_packet_size_from_header(header);

    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected a data packet");

    source = header->command.data.source;
    format = header->command.data.format;

    char* packet_error = TRY(get_morloc_data_packet_error_message, packet);
    RAISE_IF(packet_error != NULL, "\n%s", packet_error)

    switch (source) {
        case PACKET_SOURCE_MESG:
        case PACKET_SOURCE_FILE:
            // verbatim print messages and files
            TRY(print_binary, (char*)packet, packet_size);
            break;
        case PACKET_SOURCE_RPTR:
            switch (format) {
                case PACKET_FORMAT_JSON:
                case PACKET_FORMAT_MSGPACK:
                case PACKET_FORMAT_TEXT:
                case PACKET_FORMAT_DATA:
                    TRY(print_binary, (char*)packet, packet_size);
                    break;
                case PACKET_FORMAT_VOIDSTAR:
                    {
                        size_t relptr = *(size_t*)(packet + header->offset + sizeof(morloc_packet_header_t));
                        void* voidstar_ptr = TRY(rel2abs, relptr);

                        morloc_packet_header_t new_header = *header;
                        new_header.command.data.format = PACKET_FORMAT_VOIDSTAR;
                        new_header.length = TRY(calculate_voidstar_size, voidstar_ptr, schema);

                        // print header
                        TRY(print_binary, (char*)&new_header, sizeof(morloc_packet_header_t));

                        // print metadata
                        if(new_header.offset > 0) {
                            TRY(print_binary, (char*)packet + sizeof(morloc_packet_header_t), new_header.offset);
                        }

                        // print flattened voidstar data
                        TRY(print_voidstar_binary, voidstar_ptr, schema);
                    }
                    break;
            }
            break;

        default:
            RAISE("Invalid source");
    }

    return EXIT_PASS;
}

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


void close_socket(int socket_id){
    close(socket_id);
}


void close_daemon(language_daemon_t** daemon_ptr) {
    if (daemon_ptr && *daemon_ptr) {
        language_daemon_t* daemon = *daemon_ptr;

        close_socket(daemon->server_fd);

        // This list should always be empty if the run was successful
        client_list_t *current = daemon->client_fds;
        while (current) {
            client_list_t *next = current->next;
            close(current->fd);
            free(current);
            current = next;
        }

        unlink(daemon->socket_path);

        free(daemon->socket_path);
        free(daemon->tmpdir);
        free(daemon->shm_basename);

        free(daemon);
        *daemon_ptr = NULL;  // Safe nullification of caller's pointer
    }
}


// Create a Unix domain socket
static int new_socket(ERRMSG){
    INT_RETURN_SETUP

    int socket_fd = socket(AF_UNIX, SOCK_STREAM, 0);

    // AF_UNIX: Unix domain socket - other possibilities include:
    //  * AF_NET: For IPv4 Internet protocols - with SOCK_STREAM for TCP or
    //             with SOCK_DGRAM for UDP
    //  * AF_INET6: For IPv6 Internet protocols
    //  * AF_NETLINK: For kernel user interface device
    //  * AF_PACKET: For low-level packet interface

    // SOCK_STREAM - a stream socket that provides two-way, connection-based communication
    //  Alternatives include:
    //  * SOCK_DGRAM: For datagram (connectionless) sockets
    //  * SOCK_RAW: For raw network protocol access
    //  * SOCK_SEQPACKET: For sequential, reliable, connection-based packet streams

    // The 3rd argument, 0, is the protocol. For domain sockets there is only
    // one protocol, so this is always 0.

    RAISE_IF(socket_fd == EXIT_FAIL, "Error creating socket")

    return socket_fd;
}


static struct sockaddr_un new_server_addr(const char* socket_path){
    // Set up the server address structure
    struct sockaddr_un server_addr;
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sun_family = AF_UNIX;
    strncpy(server_addr.sun_path, socket_path, sizeof(server_addr.sun_path) - 1);
    return server_addr;
}


static int new_server(const char* socket_path, ERRMSG){
    INT_RETURN_SETUP

    int server_fd = TRY(new_socket);

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Remove any existing socket file
    unlink(socket_path);

    // Bind the socket to the address
    RAISE_IF_WITH(
        bind(server_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)) == EXIT_FAIL,
        close_socket(server_fd),
        "Error binding socket"
    )

    RAISE_IF_WITH(
        listen(server_fd, 1) == EXIT_FAIL,
        close_socket(server_fd),
        "Error listening on socket"
    )

    return server_fd;
}


language_daemon_t* start_daemon(
    const char* socket_path,
    const char* tmpdir,
    const char* shm_basename,
    size_t shm_default_size,
    ERRMSG
){
    PTR_RETURN_SETUP(language_daemon_t)

    language_daemon_t* daemon = (language_daemon_t*)calloc(1, sizeof(language_daemon_t));
    RAISE_IF(daemon == NULL, "Calloc for language_daemon_t failed")

    daemon->socket_path = strdup(socket_path);
    daemon->tmpdir = strdup(tmpdir);
    daemon->shm_basename = strdup(shm_basename);
    daemon->shm_default_size = shm_default_size;

    // Initialize thread-shared resources
    daemon->client_fds = NULL;  // Explicit NULL initialization
    FD_ZERO(&daemon->read_fds); // Initialize descriptor set

    // create the shared memory mappings
    daemon->shm = TRY(shinit, shm_basename, 0, shm_default_size);

    // Setup a new daemon that uses a given path for the socket address
    daemon->server_fd = TRY(new_server, socket_path);

    // Set the daemon socket to non-blocking mode (critical for pselect safety)
    int flags = fcntl(daemon->server_fd, F_GETFL);
    if (flags == -1 || fcntl(daemon->server_fd, F_SETFL, flags | O_NONBLOCK) == -1) {
        free(daemon);
        RAISE("Failed to set non-blocking mode: %s", strerror(errno));
    }

    return daemon;
}


uint8_t* stream_from_client_wait(int client_fd, int pselect_timeout_us, int recv_timeout_us, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    if (fcntl(client_fd, F_GETFD) == -1) {
        RAISE("Invalid file descriptor: %s", strerror(errno));
    }

    char* buffer = (char*)calloc(BUFFER_SIZE, sizeof(char));
    RAISE_IF(buffer == NULL, "calloc failed for buffer: %s", strerror(errno))

    fd_set read_fds;
    int max_fd = client_fd;

    // Timeout structure initialization
    struct timespec* timeout_loop_ptr = NULL;
    struct timespec ts_loop;
    if(pselect_timeout_us > 0) {
        ts_loop.tv_sec = pselect_timeout_us / 1000000;
        ts_loop.tv_nsec = (pselect_timeout_us % 1000000) * 1000;
        timeout_loop_ptr = &ts_loop;
    }

    // Signal masking setup
    sigset_t mask, origmask;
    sigemptyset(&mask);
    sigaddset(&mask, SIGINT); // do we need to add more masks here?
    sigprocmask(SIG_SETMASK, &mask, &origmask);

    // Initial receive with timeout
    FD_ZERO(&read_fds);
    FD_SET(client_fd, &read_fds);
    // int ready = pselect(max_fd + 1, &read_fds, NULL, NULL, timeout_loop_ptr, &origmask);
    int ready = pselect(max_fd + 1, &read_fds, NULL, NULL, timeout_loop_ptr, &origmask);
    sigprocmask(SIG_SETMASK, &origmask, NULL);

    RAISE_IF_WITH(ready == 0, free(buffer), "Timeout waiting for initial data");
    RAISE_IF(ready < 0, "pselect error: %s", strerror(errno));
    RAISE_IF_WITH(FD_ISSET(client_fd, &read_fds) && (errno == EBADF || errno == ECONNRESET),
                  free(buffer), "Socket error (%d): %s", client_fd, strerror(errno));

    RAISE_IF(!FD_ISSET(client_fd, &read_fds), "Bad client file descriptor")
    ssize_t recv_length = recv(client_fd, buffer, BUFFER_SIZE, 0);

    RAISE_IF_WITH(recv_length == 0, free(buffer), "Connection closed by peer: %s", strerror(errno));
    RAISE_IF_WITH(recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN,
                  free(buffer), "Recv error: %s", strerror(errno));

    size_t packet_length = TRY(morloc_packet_size, (uint8_t*)buffer);
    uint8_t* result = (uint8_t*)calloc(packet_length, sizeof(uint8_t));
    RAISE_IF(result == NULL, "calloc failure: %s", strerror(errno))

    uint8_t* data_ptr = result;
    memcpy(data_ptr, buffer, recv_length);
    data_ptr += recv_length;
    free(buffer);

    int attempts = 10;
    int initial_timeout = 10000;
    // Receive remaining data with per-operation timeout
    while ((size_t)(data_ptr - result) < packet_length) {
        bool packet_received = false;
        for(int packet_attempts = 0; packet_attempts < attempts; packet_attempts++) {
            FD_ZERO(&read_fds);
            FD_SET(client_fd, &read_fds);

            // Reset timeout for each iteration
            struct timespec ts_loop;
            if(recv_timeout_us > 0) {
                ts_loop.tv_sec = recv_timeout_us / 1000000;
                ts_loop.tv_nsec = (recv_timeout_us % 1000000) * initial_timeout * (packet_attempts + 1);
                timeout_loop_ptr = &ts_loop;
            }

            sigprocmask(SIG_SETMASK, &mask, NULL);
            ready = pselect(max_fd + 1, &read_fds, NULL, NULL, timeout_loop_ptr, &origmask);
            sigprocmask(SIG_SETMASK, &origmask, NULL);

            RAISE_IF_WITH(ready == 0, free(result), "Timeout waiting for remaining data");
            RAISE_IF(ready < 0 && errno != EINTR, "pselect error: %s", strerror(errno));
            if (ready <= 0) continue;

            if (FD_ISSET(client_fd, &read_fds)) {
                recv_length = recv(client_fd, data_ptr, BUFFER_SIZE, 0);
                if (recv_length > 0) {
                    data_ptr += recv_length;
                    packet_received = true;
                    break;
                }
                RAISE_IF_WITH(recv_length == 0, free(result), "Connection closed early: %s", strerror(errno));
                RAISE_IF_WITH(recv_length < 0 && errno != EWOULDBLOCK && errno != EAGAIN,
                              free(result), "Recv error: %s", strerror(errno));
            }
        }
        RAISE_IF(!packet_received, "Failed to retrieve packet")
    }

    return result;
}


// Stream with eternal wait
uint8_t* stream_from_client(int client_fd, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(stream_from_client_wait, client_fd, 0, 0);
    return packet;
}


uint8_t* send_and_receive_over_socket_wait(const char* socket_path, const uint8_t* packet, int pselect_timeout_us, int recv_timeout_us, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    int client_fd = TRY(new_socket);

    struct sockaddr_un server_addr = new_server_addr(socket_path);

    // Data packet to return
    uint8_t* result = NULL;

    // Connect to the server
    int retcode = -1;
    WAIT( { retcode = connect(client_fd, (struct sockaddr*)&server_addr, sizeof(server_addr)); },
          retcode == 0,
          RAISE_WITH(close_socket(client_fd), "Failed to connect to pipe '%s', ran out of time", socket_path)
        )

    size_t packet_size = TRY(morloc_packet_size, packet);

    // Send a message and wait for a reply from the nexus
    ssize_t bytes_sent = 0;

    WAIT( { bytes_sent = send(client_fd, packet, packet_size, MSG_NOSIGNAL); },
          (size_t)bytes_sent == packet_size,
          RAISE_WITH(close_socket(client_fd), "Failed to send data to '%s', ran out of time", socket_path)
        )

    result = stream_from_client_wait(client_fd, pselect_timeout_us, recv_timeout_us, &CHILD_ERRMSG);
    RAISE_IF_WITH(CHILD_ERRMSG != NULL, close_socket(client_fd), "Failed to read data returned from pipe '%s'\n%s", socket_path, CHILD_ERRMSG);

    close_socket(client_fd);

    return result;
}


uint8_t* send_and_receive_over_socket(const char* socket_path, const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* result = TRY(send_and_receive_over_socket_wait, socket_path, packet, 0, 0);
    return result;
}


size_t send_packet_to_foreign_server(int client_fd, uint8_t* packet, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0)

    size_t size = TRY(morloc_packet_size, packet);

    ssize_t bytes_sent = 0;
    bytes_sent = send(client_fd, packet, size, MSG_NOSIGNAL);

    RAISE_IF(bytes_sent < 0, "Failed to send over client %d: %s", client_fd, strerror(errno))
    RAISE_IF((size_t)bytes_sent != size, "Partial send over client %d, only sent %zd of %zu bytes: %s", client_fd, bytes_sent, size, strerror(errno))

    return bytes_sent;
}

int wait_for_client_with_timeout(language_daemon_t* daemon, int timeout_us, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)

    // clear the list of file descriptors, these will be re-added below
    FD_ZERO(&daemon->read_fds);

    // add the server file descriptor to the fds_set
    FD_SET(daemon->server_fd, &daemon->read_fds);

    int max_fd = daemon->server_fd;

    client_list_t* client_fds;

    // loop through all pre-existing the client file descriptors stored in the daemon list
    for(client_fds = daemon->client_fds; client_fds != NULL; client_fds = client_fds->next){
        FD_SET(client_fds->fd, &daemon->read_fds);
        max_fd = max_fd > client_fds->fd ? max_fd : client_fds->fd;
    }

    // Modified WAIT block using pselect

    // Timeout structure initialization
    struct timespec* timeout_ptr = NULL;
    struct timespec ts_loop;
    if(timeout_us > 0) {
        ts_loop.tv_sec = timeout_us / 1000000;
        ts_loop.tv_nsec = (timeout_us % 1000000) * 1000;
        timeout_ptr = &ts_loop;
    }

    int ready;
    sigset_t emptymask;
    sigemptyset(&emptymask);
    // `pselect` will until timeout for something to crawl out of the pipe.
    // But if the pipe itself is missing or broken (for example, if the socket
    // file has not yet been written), then select dies immediately, for this
    // reason it is wrapped in WAIT to retry for a few minutes for giving up the
    // ghost for good.
    WAIT( { ready = pselect(max_fd + 1, &daemon->read_fds, NULL, NULL, timeout_ptr, &emptymask); },
          ready >= 0,
          RAISE("Failed to read data")
        )

    // if pselect timed out, return 0
    if(ready == 0){
        return ready;
    }

    int selected_fd = -1;

    // get the new client that select found and add it to the end of the client list
    if (FD_ISSET(daemon->server_fd, &daemon->read_fds)) {
        selected_fd = accept(daemon->server_fd, NULL, NULL);
        if (selected_fd > 0) {
            fcntl(selected_fd, F_SETFL, O_NONBLOCK);

            // Create the new client
            client_list_t* new_client = (client_list_t*)calloc(1, sizeof(client_list_t));
            new_client->fd = selected_fd;
            new_client->next = NULL;

            if(daemon->client_fds == NULL){
                daemon->client_fds = new_client;
            } else {
                client_list_t* last = daemon->client_fds;
                while (last->next) last = last->next;  // Find last node
                last->next = new_client;
            }
        } else if (errno != EAGAIN && errno != EWOULDBLOCK) {
            RAISE("Error accepting client connection");
        }
    }

    client_fds = daemon->client_fds;
    int return_fd = client_fds->fd;
    daemon->client_fds = daemon->client_fds->next;
    free(client_fds);

    return return_fd;
}

// wait forever
int wait_for_client(language_daemon_t* daemon, ERRMSG) {
    VAL_RETURN_SETUP(int, -1)
    int return_fd = TRY(wait_for_client_with_timeout, daemon, 0);
    return return_fd;
}

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
static uint64_t mix(uint64_t a, uint64_t b) {
    a ^= b * PRIME64_1;
    a = (a << 31) | (a >> (64 - 31)); // Rotate
    a *= PRIME64_2;
    return a;
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

                uint8_t* element_data = TRY((uint8_t*)rel2abs, array->data);

                if (schema_is_fixed_width(schema)){
                    size_t array_size = element_width * array->size;
                    hash = XXH64(element_data, array_size, seed);
                } else {
                    for(size_t i = 0; i < array->size; i++){
                        hash = TRY(hash_voidstar, element_data + i * element_width, schema->parameters[0], hash);
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
                        hash = TRY(hash_voidstar, element_data + schema->offsets[i], schema->parameters[i], hash);
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
            uint64_t arg_hash = TRY(hash_voidstar, (void*)(arg_data + arg_start), schema, 0);
            *hash = mix(*hash, arg_hash);
        }
    } else if (command_type == PACKET_TYPE_DATA){
        uint8_t* voidstar = TRY(get_morloc_data_packet_value, packet + sizeof(morloc_packet_header_t) + header->offset, schema);
        *hash = TRY(hash_voidstar, (void*)voidstar, schema, seed);
    } else {
        RAISE("Cannot hash packet with command 0x%02hhxh", command_type)
    }

    return true; // success
}


static char* make_cache_filename_ext(uint64_t key, const char* cache_path, const char* ext, ERRMSG) {
    PTR_RETURN_SETUP(char)

    char buffer[MAX_FILENAME_SIZE] = { '\0' };

    // Format the filename with the key in hexadecimal
    //  * It is always padded to 16 characters
    //  * The PRIx64 macro from inttypes.h ensures portability since the
    //    uint64_t type may be aliased to different 64 bit types on
    //    different systems (e.g., unsigned long or unsigned long long)
    int written = snprintf(buffer, sizeof(buffer), "%s/%016" PRIx64 "%s", cache_path, key, ext);

    RAISE_IF((size_t)written == 0, "Failed to create filename")

    return strndup(buffer, sizeof(buffer)-1);
}


static char* make_cache_filename(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)
    char packet_ext[] = ".packet";
    char* filename = TRY(make_cache_filename_ext, key, cache_path, packet_ext);
    return filename;
}


static char* make_cache_data_filename(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)
    char dat_ext[] = ".dat";
    char* filename = TRY(make_cache_filename_ext, key, cache_path, dat_ext);
    return filename;
}


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
char* put_cache_packet(const uint8_t* voidstar, const Schema* schema, uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)

    // Generate the cache filename
    char* packet_filename = TRY(make_cache_filename, key, cache_path)

    // Generate the data filename
    char* data_filename = TRY(make_cache_data_filename, key, cache_path)

    uint8_t* data_packet = make_mpk_data_packet(data_filename, schema);

    size_t data_packet_size = TRY(morloc_packet_size, data_packet);

    // convert voidstar data to MessagePack
    char* mpk_data = NULL;
    size_t mpk_size = 0;
    TRY(pack_with_schema, voidstar, schema, &mpk_data, &mpk_size);

    // write packet
    TRY_WITH(free(data_packet), write_atomic, packet_filename, data_packet, data_packet_size);
    free(data_packet);

    TRY_WITH(free(mpk_data), write_atomic, data_filename, (uint8_t*)mpk_data, mpk_size);
    free(mpk_data);

    return strdup(packet_filename);
}


// Get a cached packet given the key (usually a hash)
//
// The cached packet should be stateless, independent of the shared memory pool
// (which is local and transient). The packet should either store its contents
// as raw data or should contain a path to a file with the raw data.
uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    char* filename = TRY(make_cache_filename, key, cache_path)

    // Read the binary file into memory
    size_t file_size;
    uint8_t* data = TRY(read_binary_file, filename, &file_size);

    return data;
}


// Deletes a cached packet given a key
//
// Also deletes the associated data file, if any is defined
bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    BOOL_RETURN_SETUP

    // Generate the cache filename
    char* filename = TRY(make_cache_filename, key, cache_path)

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
    char* filename = TRY(make_cache_filename, key, cache_path)

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

// {{{ parsing CLI arguments

// Check if the data may be MessagePack (and is not JSON)
//
// We look ONLY at the first character and the size
static bool maybe_msgpack(const uint8_t* data, size_t size) {
    if (size < 1) return false;

    // Check for MessagePack's initial byte patterns
    uint8_t c = data[0];

    // MessagePack stores the integers 0 to 127 in the characters 0x7f, which
    // coincide with the ASCII characters. JSON starts with whitespace, 't'
    // (true), 'f' (false), 'n', (nil), quotes, '[', or '{'. A complete
    // MessagePack message that begins with a FixInt must end there (it isn't an
    // array).

    // Trouble is that 0x30-0x39 may be either 0-9 in JSON or 48-57 in
    // MessagePack. That said, JSON does often have a newline. And I can ensure
    // that Morloc-produced JSON always does. So perhaps it is still safe assume
    // that no JSON file can be a single character.

    return c > 0x7f || (c <= 0x7f && size == 1);
}


int rebase_relative_pointers(absptr_t mem, relptr_t rel, const Schema* schema, ERRMSG){
    INT_RETURN_SETUP

    switch(schema->type){
        case MORLOC_STRING:
            {
                Array* arr = (Array*)mem;
                arr->data += rel;
            }
            break;
        case MORLOC_ARRAY:
            {
                Array* arr = (Array*)mem;
                arr->data += rel;
                if (!schema_is_fixed_width(schema->parameters[0])){
                    absptr_t element_data = TRY(rel2abs, arr->data);
                    for(size_t i = 0; i < arr->size; i++){
                        absptr_t element_ptr = (absptr_t)((char*)element_data + i * schema->parameters[0]->width);
                        TRY(rebase_relative_pointers, element_ptr, rel, schema->parameters[0]);
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                if (! schema_is_fixed_width(schema)){
                    for(size_t i = 0; i < schema->size; i++){
                        if (! schema_is_fixed_width(schema->parameters[i])){
                            absptr_t element_ptr = (absptr_t)((char*)mem + schema->offsets[i]);
                            TRY(rebase_relative_pointers, element_ptr, rel, schema->parameters[i]);
                        } else {
                        }
                    }
                }
            }
            break;
        default:
            break;
    }

    return EXIT_PASS;
}

// Parse a command line argument string that should contain data of a given type
uint8_t* parse_cli_data_argument(char* arg, const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    FILE* fd;
    uint8_t* packet;
    void* voidstar = NULL;

    // handle STDIN
    if(strcmp(arg, "/dev/stdin") == 0 || strcmp(arg, "-") == 0){
        fd = stdin;
    }
    // If the argument is a file, try to open it and parse it as JSON or MessagePack
    else if(file_exists(arg)){
        fd = fopen(arg, "rb");
        RAISE_IF(fd == NULL, "The argument '%s' is a filename, but it can't be read:\n%s", arg, strerror(errno));
    } else {
        fd = NULL; // this argument is not a file
    }

    if(fd == NULL){
        // The argument is the data
        // JSON is currently the only supported option
        relptr_t packet_ptr = read_json_with_schema(arg, schema, &CHILD_ERRMSG);
        packet = make_standard_data_packet(packet_ptr, schema);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read argument:\n%s", CHILD_ERRMSG)
        return packet;
    } else {
        // The argument is a file
        size_t data_size = 0;
        char* data = (char*)read_binary_fd(fd, &data_size, &CHILD_ERRMSG);
        if(fd != stdin){
            fclose(fd);
        }
        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "\n%s", CHILD_ERRMSG)

        if(has_suffix(arg, ".json")){
            relptr_t packet_ptr = read_json_with_schema(data, schema, &CHILD_ERRMSG);
            packet = make_standard_data_packet(packet_ptr, schema);
            // If this isn't a JSON file, but you say it is, then either you are
            // confused or evil. In either case, I'll just play it safe and die.
            RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "Failed to read json argument file '%s':\n%s", arg, CHILD_ERRMSG)
            return packet;
        }

        if(has_suffix(arg, ".mpk") || has_suffix(arg, ".msgpack")){
            unpack_with_schema(data, data_size, schema, &voidstar, &CHILD_ERRMSG);
            RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "Failed to read MessagePack argument file '%s':\n%s", arg, CHILD_ERRMSG)
            relptr_t relptr = TRY(abs2rel, voidstar);
            packet = make_standard_data_packet(relptr, schema);
            return packet;
        }

        // If the extension is not recognized
        // First try to read it as a morloc voidstar packet if the data is at
        // least as large as a header
        if(data_size >= sizeof(morloc_packet_header_t)){
            morloc_packet_header_t* header = read_morloc_packet_header((uint8_t*)data, &CHILD_ERRMSG);
            if(CHILD_ERRMSG == NULL && header != NULL){
                uint8_t source = header->command.data.source;
                uint8_t format = header->command.data.format;

                if (source == PACKET_SOURCE_RPTR) {
                    if (format == PACKET_FORMAT_VOIDSTAR) {
                        // get a pointer to the payload start
                        void* voidstar_ptr = (void*)(data + sizeof(morloc_packet_header_t) + header->offset);

                        // copy the data to shared memory
                        absptr_t mem = TRY(shmemcpy, voidstar_ptr, header->length);
                        free(data);

                        // Get the relative pointer to the beginning of the
                        // loaded data. This will be the offset by which we need
                        // to adjust all realtive pointers in the uploaded
                        // voidstar.
                        relptr_t relptr = TRY(abs2rel, mem);

                        // set all relative pointers
                        TRY(rebase_relative_pointers, mem, relptr, schema);

                        packet = make_standard_data_packet(relptr, schema);

                        return (uint8_t*)packet;
                    }
                    RAISE("For RPTR source, expected voidstar format, found: 0x%02hhx", format);
                }
                return (uint8_t*)data;
            }
        }

        // Next check if it is MessagePack
        if(maybe_msgpack((uint8_t*)data, data_size)){
            // Try to parse as MessagePack
            TRY(unpack_with_schema, data, data_size, schema, &voidstar);
            relptr_t relptr = TRY(abs2rel, voidstar);
            packet = make_standard_data_packet(relptr, schema);
            free(data);
            return (uint8_t*)packet;
        }

        // Try to parse as JSON
        relptr_t packet_ptr = read_json_with_schema(data, schema, &CHILD_ERRMSG);
        if(CHILD_ERRMSG == NULL){
            free(data);
            packet = make_standard_data_packet(packet_ptr, schema);
            return packet;
        }

        free(data);
        RAISE("Failed to read argument from file '%s'", arg)
    }
}

// Given the manifold ID and argument and schema strings, create a morloc call packet
uint8_t* make_call_packet_from_cli(
    uint32_t mid,
    char** args, // NULL terminated array of argument strings
    const char** arg_schema_strs, // NULL terminated array of schema strings
    ERRMSG
){
    PTR_RETURN_SETUP(uint8_t)

    size_t nschemas = 0;
    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        nschemas++;
    }

    const Schema** schemas = (const Schema**)calloc(nschemas + 1, sizeof(Schema*));
    RAISE_IF(schemas == NULL, "Failed to allocate memory for schemas")

    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        schemas[i] = parse_schema(&arg_schema_strs[i], &CHILD_ERRMSG);
        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(schemas), "Failed to parse argument %zu:\n%s", i, CHILD_ERRMSG)
    }
    schemas[nschemas] = NULL;

    size_t nargs = 0;
    for(size_t i = 0; args[i] != NULL; i++){
        nargs++;
    }

    const uint8_t** packet_args = (const uint8_t**)calloc(nargs, sizeof(uint8_t*));
    RAISE_IF_WITH(packet_args == NULL, free(schemas), "Failed to allocate packet_args");

    for(size_t i = 0; i < nargs; i++){
        packet_args[i] = parse_cli_data_argument(args[i], schemas[i], &CHILD_ERRMSG);
        if(CHILD_ERRMSG != NULL){
            free(schemas);
            free(packet_args);
            RAISE("Failed to parse argument %zu ('%s')\n%s", i, args[i], CHILD_ERRMSG);
        }
    }

    uint8_t* call_packet = make_morloc_local_call_packet(mid, packet_args, nargs, &CHILD_ERRMSG);
    if(CHILD_ERRMSG != NULL){
        free(schemas);
        free(packet_args);
        RAISE("Failed to make call packet:\n%s", CHILD_ERRMSG);
    }

    free(schemas);
    free(packet_args);
    return call_packet;
}

/// }}}

// {{{ pure morloc types and constructors

// all morloc expression types
typedef enum {
  MORLOC_X_DAT,
  MORLOC_X_APP,
  MORLOC_X_LAM,
  MORLOC_X_BND,
  MORLOC_X_PAT,
} morloc_expression_type;

// application types
typedef enum { APPLY_PATTERN, APPLY_LAMBDA } morloc_app_expression_type;

// pattern types
typedef enum { SELECT_BY_KEY, SELECT_BY_INDEX, SELECT_END } morloc_pattern_type;

// Forward declarations
typedef struct morloc_expression_s morloc_expression_t;
typedef struct morloc_app_expression_s morloc_app_expression_t;
typedef struct morloc_lam_expression_s morloc_lam_expression_t;
typedef struct morloc_data_s morloc_data_t;
typedef struct morloc_pattern_s morloc_pattern_t;
typedef struct morloc_bnd_expression_s morloc_bnd_expression_t;

// represent a pure morloc expression, a node in the syntax tree
typedef struct morloc_expression_s {
    morloc_expression_type type;
    union {
        morloc_app_expression_t* app_expr;
        morloc_lam_expression_t* lam_expr;
        morloc_bnd_expression_t* bnd_expr;
        morloc_pattern_t* pattern_expr;
        morloc_data_t* data_expr;
    } expr;
} morloc_expression_t;

typedef struct morloc_bnd_expression_s {
  char* varname;
  Schema* schema;
} morloc_bnd_expression_t;

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
    Schema* schema;
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
      char** keys;
      size_t* indices;
    };
   morloc_pattern_t* selectors;
} morloc_pattern_t;

morloc_expression_t* make_morloc_bound_var(const char* schema_str, char* varname){
    char* error_msg = NULL;
    Schema* schema = parse_schema(&schema_str, &error_msg);
    // careful here, I'm ignoring the error

    morloc_bnd_expression_t* bndvar = (morloc_bnd_expression_t*)calloc(1, sizeof(morloc_bnd_expression_t));
    bndvar->varname = varname;
    bndvar->schema = schema;

    morloc_expression_t* expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    expr->type = MORLOC_X_BND;
    expr->expr.bnd_expr = bndvar;

    return expr;
}

morloc_expression_t* make_morloc_literal(
  const char* schema_str,
  primitive_t lit
){
    char* error_msg = NULL;
    Schema* schema = parse_schema(&schema_str, &error_msg);
    morloc_data_t* data = (morloc_data_t*)malloc(sizeof(morloc_data_t));

    data->is_voidstar = false;
    data->data.lit_val = lit;
    data->schema = schema;

    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->type = MORLOC_X_DAT;
    expr->expr.data_expr = data;
    return expr;
}

morloc_expression_t* make_morloc_container(
  const char* schema_str,
  size_t nargs,
  ... // list of container elements
){
    va_list value_list;
    va_start(value_list, nargs);
    char* error_msg = NULL;
    Schema* schema = parse_schema(&schema_str, &error_msg);
    morloc_data_t* data = (morloc_data_t*)malloc(sizeof(morloc_data_t));
    data->schema = schema;
    data->is_voidstar = false;

    morloc_expression_t** values = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
    for(size_t i = 0; i < nargs; i++){
        values[i] = va_arg(value_list, morloc_expression_t*);
    }

    // Set the appropriate container field based on schema type
    switch(schema->type) {
        case MORLOC_ARRAY: {
            morloc_data_array_t* array = (morloc_data_array_t*)malloc(sizeof(morloc_data_array_t));
            array->schema = schema->parameters[0];
            array->size = nargs;
            array->values = values;
            data->data.array_val = array;
            break;
        }
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            data->data.tuple_val = values;
            break;
        default:
            // Schema type is not a container type
            return NULL;
    }
    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->type = MORLOC_X_DAT;
    expr->expr.data_expr = data;

    va_end(value_list);

    return expr;
}

morloc_expression_t* make_morloc_app(
  morloc_expression_t* func,
  size_t nargs,
  ... // list of input arguments
){
    va_list args;
    va_start(args, nargs);

    morloc_app_expression_t* app = (morloc_app_expression_t*)malloc(sizeof(morloc_app_expression_t));
    // Determine application type based on func
    switch(func->type) {
        case MORLOC_X_PAT:
            app->type = APPLY_PATTERN;
            app->function.pattern = func->expr.pattern_expr;
            break;
        case MORLOC_X_LAM:
            app->type = APPLY_LAMBDA;
            app->function.lambda = func->expr.lam_expr;
            break;
        default:
            // Can only apply pattern or lambda
            return NULL;
    }

    app->args = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
    for(size_t i = 0; i < nargs; i++){
        app->args[i] = va_arg(args, morloc_expression_t*);
    }
    app->nargs = nargs;

    morloc_expression_t* expr = (morloc_expression_t*)malloc(sizeof(morloc_expression_t));
    expr->type = MORLOC_X_APP;
    expr->expr.app_expr = app;

    va_end(args);

    return expr;
}

morloc_expression_t* make_morloc_lambda(
  morloc_expression_t* body,
  size_t nvars,
  ... // list of input variable names
){
    va_list var_list;
    va_start(var_list, nvars);

    char** vars = (char**)calloc(nvars, sizeof(char*));
    for(size_t i = 0; i < nvars; i++){
        vars[i] = va_arg(var_list, char*);
    }

    morloc_lam_expression_t* lam = (morloc_lam_expression_t*)malloc(sizeof(morloc_lam_expression_t));
    lam->nargs = nvars;
    lam->args = vars;
    lam->body = body;

    va_end(var_list);

    morloc_expression_t* lam_expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
    lam_expr->type = MORLOC_X_LAM;
    lam_expr->expr.lam_expr = lam;

    return lam_expr;
}

morloc_pattern_t* make_key_selector(
  char** keys,
  morloc_pattern_t* vals,
  size_t nargs
){
    morloc_pattern_t* pattern = (morloc_pattern_t*)malloc(sizeof(morloc_pattern_t));
    pattern->type = SELECT_BY_KEY;
    pattern->size = nargs;
    pattern->keys = keys;
    pattern->selectors = vals;
    return pattern;
}

morloc_pattern_t* make_index_selector(
  size_t* indices,
  morloc_pattern_t* vals,
  size_t nargs
){
    morloc_pattern_t* pattern = (morloc_pattern_t*)malloc(sizeof(morloc_pattern_t));
    pattern->type = SELECT_BY_INDEX;
    pattern->size = nargs;
    pattern->indices = (size_t*)indices;
    pattern->selectors = vals;
    return pattern;
}

// }}}

// {{{ pure morloc interpretor

static absptr_t morloc_eval_r(morloc_expression_t* expr, absptr_t dest, size_t width, dict_t* bndvars, ERRMSG);
static morloc_expression_t* apply_getter( morloc_expression_t* expr, morloc_pattern_t* pattern, ERRMSG);
static morloc_expression_t* apply_setter( morloc_expression_t* expr, morloc_pattern_t* pattern, morloc_expression_t** args, size_t nargs, ERRMSG);

// evaluate a pure morloc expression with user provided arguments
absptr_t morloc_eval(
  morloc_expression_t* expr,
  uint8_t** arg_voidstar, // voidstar data
  Schema** arg_schemas, // argument schema strings
  size_t nargs,
  ERRMSG
) {
    PTR_RETURN_SETUP(absptr_t)
    morloc_expression_t* new_expr = NULL;

    // If the top expression is a lambda, then the arguments must be the user
    // provided arguments. The arguments are given as voidstar values. We need
    // to make a new application expression that wraps all the voidstar values
    // and applies them to the lambda function.
    if (expr->type == MORLOC_X_LAM) {
        morloc_expression_t** arg_exprs = (morloc_expression_t**)calloc(nargs, sizeof(morloc_expression_t*));
        for(size_t i = 0; i < nargs; i++){
            arg_exprs[i] = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
            arg_exprs[i]->type = MORLOC_X_DAT;
            arg_exprs[i]->expr.data_expr = (morloc_data_t*)calloc(1, sizeof(morloc_data_t));
            arg_exprs[i]->expr.data_expr->schema = arg_schemas[i];
            arg_exprs[i]->expr.data_expr->is_voidstar = true;
            arg_exprs[i]->expr.data_expr->data.voidstar = arg_voidstar[i];
        }

        morloc_app_expression_t* app_expr = (morloc_app_expression_t*)calloc(1, sizeof(morloc_app_expression_t));
        app_expr->type = APPLY_LAMBDA;
        app_expr->function.lambda = expr->expr.lam_expr;
        app_expr->args = arg_exprs;
        app_expr->nargs = nargs;

        new_expr = (morloc_expression_t*)calloc(1, sizeof(morloc_expression_t));
        new_expr->type = MORLOC_X_APP;
        new_expr->expr.app_expr = app_expr;
    }
    // If we are not dealing with a lambda, we should instead directly evaluate
    // the input expression
    else {
        new_expr = expr;
    }

    absptr_t result = TRY(morloc_eval_r, new_expr, NULL, 0, NULL);

    return result;
}

static absptr_t morloc_eval_r(morloc_expression_t* expr, absptr_t dest, size_t width, dict_t* bndvars, ERRMSG) {
    PTR_RETURN_SETUP(void)

    RAISE_IF(!expr, "Empty expression")

    switch(expr->type) {
        case MORLOC_X_DAT: {
            morloc_data_t* data = expr->expr.data_expr;

            // directly return voidstar data
            if (data->is_voidstar) {
                return data->data.voidstar;
            }

            if (dest == NULL){
                dest = TRY(shcalloc, 1, data->schema->width);
                width = data->schema->width;
            } else {
                RAISE_IF(width != data->schema->width, "Unexpected data size")
            }

            switch(data->schema->type){
                 case MORLOC_NIL:
                     memcpy(dest, (void*)(&data->data.lit_val.z), width);
                     break;
                 case MORLOC_BOOL:
                     memcpy(dest, (void*)(&data->data.lit_val.b), width);
                     break;
                 case MORLOC_SINT8:
                     memcpy(dest, (void*)(&data->data.lit_val.i1), width);
                     break;
                 case MORLOC_SINT16:
                     memcpy(dest, (void*)(&data->data.lit_val.i2), width);
                     break;
                 case MORLOC_SINT32:
                     memcpy(dest, (void*)(&data->data.lit_val.i4), width);
                     break;
                 case MORLOC_SINT64:
                     memcpy(dest, (void*)(&data->data.lit_val.i8), width);
                     break;
                 case MORLOC_UINT8:
                     memcpy(dest, (void*)(&data->data.lit_val.u1), width);
                     break;
                 case MORLOC_UINT16:
                     memcpy(dest, (void*)(&data->data.lit_val.u2), width);
                     break;
                 case MORLOC_UINT32:
                     memcpy(dest, (void*)(&data->data.lit_val.u4), width);
                     break;
                 case MORLOC_UINT64:
                     memcpy(dest, (void*)(&data->data.lit_val.u8), width);
                     break;
                 case MORLOC_FLOAT32:
                     memcpy(dest, (void*)(&data->data.lit_val.f4), width);
                     break;
                 case MORLOC_FLOAT64:
                     memcpy(dest, (void*)(&data->data.lit_val.f8), width);
                     break;
                 case MORLOC_STRING:
                     {
                         char* str = data->data.lit_val.s;
                         size_t str_size = strlen(str);
                         relptr_t str_relptr = -1;
                         if(str_size > 0){
                             absptr_t str_absptr = TRY(shmemcpy, (void*)str, str_size);
                             str_relptr = TRY(abs2rel, str_absptr);
                         }
                         Array str_array;
                         str_array.size = str_size;
                         str_array.data = str_relptr;
                         memcpy(dest, (void*)(&str_array), width);
                     }
                     break;
                 case MORLOC_ARRAY:
                     {
                         morloc_data_array_t* arr = data->data.array_val;
                         size_t arr_size = arr->size;
                         size_t element_width = arr->schema->width;
                         relptr_t arr_reldata = -1;
                         if(arr_size > 0){
                             absptr_t arr_data = TRY(shcalloc, arr_size, element_width);
                             for(size_t i = 0; i < arr_size; i++){
                                 TRY(morloc_eval_r, arr->values[i], arr_data + i * element_width, element_width, bndvars)
                             }
                             arr_reldata = TRY(abs2rel, arr_data)
                         }
                         Array array;
                         array.size = arr_size;
                         array.data = arr_reldata;
                         memcpy(dest, (void*)(&array), width);
                     }
                     break;
                 case MORLOC_TUPLE:
                 case MORLOC_MAP:
                     {
                         size_t element_width = 0;
                         for(size_t i = 0; i < data->schema->size; i++){
                             element_width = data->schema->parameters[i]->width;
                             morloc_expression_t* element = data->data.tuple_val[i];
                             absptr_t element_dest = dest + data->schema->offsets[i];
                             TRY(morloc_eval_r, element, element_dest, element_width, bndvars)
                         }
                     }
                     break;

                 default:
                     RAISE("Illegal value in enum")
            }

        } break;

        case MORLOC_X_APP: {
            // Application: apply function to arguments
            morloc_app_expression_t* app = expr->expr.app_expr;

            switch(app->type) {
                case APPLY_PATTERN:
                    // TODO: Apply pattern to arguments
                    // return apply_getter(...);
                    RAISE("Patterns not yet supported")

                case APPLY_LAMBDA: {
                    morloc_lam_expression_t* lam = app->function.lambda;
                    absptr_t* arg_results = (absptr_t*)calloc(app->nargs, sizeof(absptr_t));

                    // evaluate all arguments outside the new lambda scope
                    for(size_t i = 0; i < app->nargs; i++){
                        arg_results[i] = TRY(morloc_eval_r, app->args[i], NULL, 0, bndvars);
                    }

                    // remove shadowed values from the variable table
                    for(size_t i = 0; i < app->nargs; i++){
                        bndvars = dict_delete(lam->args[i], bndvars);
                    }

                    // add evaluated arguments to the variable table
                    for(size_t i = 0; i < app->nargs; i++){
                        bndvars = dict_insert(lam->args[i], arg_results[i], bndvars);
                    }

                    // evaluate the lambda body with the new variable table
                    dest = TRY(morloc_eval_r, lam->body, dest, width, bndvars);
                } break;
                default:
                    RAISE("Invalid functional term")
            }
        } break;

        case MORLOC_X_BND: {
            morloc_bnd_expression_t* bndvar = expr->expr.bnd_expr;
            absptr_t bnd_result = dict_lookup(bndvar->varname, bndvars);
            RAISE_IF(bnd_result == NULL, "Unbound variable %s", bndvar->varname);
            if(dest == NULL){
                dest = bnd_result;
            } else {
                memcpy(dest, bnd_result, bndvar->schema->width);
            }
        } break;

        default:
            RAISE("Illegal top expression");
    }

    return dest;
}

static morloc_expression_t* apply_getter(
  morloc_expression_t* expr,
  morloc_pattern_t* pattern,
  ERRMSG
) {
    PTR_RETURN_SETUP(morloc_expression_t)
    if (!expr || !pattern) return NULL;

    switch(pattern->type) {
        case SELECT_BY_KEY:
            // Extract fields by key from tuple/map
            // TODO: Check expr is MORLOC_X_DAT with tuple_val or map_val
            // TODO: For each key in pattern->keys, extract corresponding value
            // TODO: Recursively apply pattern->selectors if present
            return NULL;

        case SELECT_BY_INDEX:
            // Extract elements by index from tuple/array
            // TODO: Check expr is MORLOC_X_DAT with tuple_val or array_val
            // TODO: For each index in pattern->indices, extract corresponding value
            // TODO: Recursively apply pattern->selectors if present
            return NULL;

        case SELECT_END:
            // End of pattern - return expr as-is
            return expr;

        default:
            RAISE("Illegal pattern enum value");
    }
}

static morloc_expression_t* apply_setter(
  morloc_expression_t* expr,
  morloc_pattern_t* pattern,
  morloc_expression_t** args,
  size_t nargs,
  ERRMSG
) {
    PTR_RETURN_SETUP(morloc_expression_t)
    if (!expr || !pattern) return NULL;

    switch(pattern->type) {
        case SELECT_BY_KEY:
            // Set fields by key in tuple/map
            // TODO: Check expr is MORLOC_X_DAT with tuple_val or map_val
            // TODO: Check nargs matches pattern->size
            // TODO: For each key in pattern->keys, set corresponding value from args
            // TODO: Recursively apply pattern->selectors if present
            return NULL;

        case SELECT_BY_INDEX:
            // Set elements by index in tuple/array
            // TODO: Check expr is MORLOC_X_DAT with tuple_val or array_val
            // TODO: Check nargs matches pattern->size
            // TODO: For each index in pattern->indices, set corresponding value from args
            // TODO: Recursively apply pattern->selectors if present
            return NULL;

        case SELECT_END:
            // End of pattern - should not be called with setter
            // TODO: Handle error
            return NULL;

        default:
            RAISE("Invalid pattern type");
    }
}

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
size_t parse_slurm_time(const char* time_str, ERRMSG) {
    VAL_RETURN_SETUP(size_t, 0)

    int days = 0;
    int hours = 0;
    int minutes = 0;
    int seconds = 0;

    int n;
    errno = 0;
    n = sscanf(time_str, "%d-%02d:%02d:%02d", &days, &hours, &minutes, &seconds);
    // if not all of the 4 values where parsed, try the day-less format
    if(n != 4){
        days = 0;
        n = sscanf(time_str, "%02d:%02d:%02d", &hours, &minutes, &seconds);
        // if not all of the 3 values where parsed, die
        if(n != 3){
            RAISE("Failed to scan slurm walltime string '%s'", time_str);
        }
    }

    // Check for negative time
    if(days < 0 || hours < 0 || minutes < 0 || seconds < 0) {
        RAISE("Negative time component in '%s'", time_str);
    }

    // Check for non-canonical time specifications
    if(hours > 23 || minutes > 59 || seconds > 59) {
        RAISE("Invalid time component in '%s' (HH<=23 MM<=59 SS<=59)", time_str);
    }

    // Specifying too many days is probably a mistake and could lead to overflows
    if(days > 3650){
        RAISE("Do you really want to run this job for more than 10 years?")
    }

    return seconds + 60*minutes + 60*60*hours + 60*60*24*days;
}

// Convert a number of seconds to a walltime string of format:
//   days-hours:minutes:seconds
// hours, minutes, and seconds should be 0-padded to 2 digits
char* write_slurm_time(int seconds){

    int days = seconds / (60 * 60 * 24);
    seconds -= days * 60 * 60 * 24;

    int hours = seconds / (60 * 60);
    seconds -= hours * 60 * 60;

    int minutes = seconds / 60;
    seconds -= minutes * 60;

    char* time_str = (char*)calloc(32, sizeof(char));
    snprintf(time_str, 32, "%d-%02d:%02d:%02d", days, hours, minutes, seconds);

    return time_str;
}

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

    pos = sizeof(morloc_packet_header_t) + (size_t)header->offset;
    for(size_t i = 0; i < *nargs; i++){
        uint8_t* arg = packet + pos;
        morloc_packet_header_t* arg_header = (morloc_packet_header_t*)arg;
        args[i] = packet + pos;
        pos += sizeof(morloc_packet_header_t) + arg_header->offset + arg_header->length;
    }

    return true;
}


// Check if a given SLURM job has completed
bool slurm_job_is_complete(uint32_t job_id) {
    char cmd[256];
    snprintf(cmd, sizeof(cmd), "sacct -j %u --format=State --noheader", job_id);

    FILE *sacct = popen(cmd, "r");
    if (!sacct) return false;

    char state[16];
    bool done = false;
    while (fgets(state, sizeof(state), sacct)) {
        if (strstr(state, "COMPLETED") ||
            strstr(state, "FAILED") ||
            strstr(state, "CANCELLED")) {
            done = true;
            break;
        }
    }
    pclose(sacct);
    return done;
}


uint32_t submit_morloc_slurm_job(
    const char* nexus_path,
    const char* socket_basename,
    const char* call_packet_filename,
    const char* result_cache_filename,
    const char* output_filename,
    const char* error_filename,
    const resources_t* resources,
    ERRMSG)
{
    VAL_RETURN_SETUP(uint32_t, 0)

    RAISE_IF(nexus_path == NULL, "nexus path undefined")
    RAISE_IF(socket_basename == NULL, "socket basename undefined")
    RAISE_IF(call_packet_filename == NULL, "call packet filename undefined")
    RAISE_IF(result_cache_filename == NULL, "result cache filename undefined")
    RAISE_IF(output_filename == NULL, "slurm output filename undefined")
    RAISE_IF(error_filename == NULL, "slurm error filename undefined")

    char cmd[MAX_SLURM_COMMAND_LENGTH];
    FILE *slurm;
    uint32_t job_id = 0;

    // Build resource parameters
    char* time_str = write_slurm_time(resources->time); // DD-HH:MM:SS
    char resources_spec[256];
    snprintf(resources_spec, sizeof(resources_spec),
        "--mem=%dG --time=%s --cpus-per-task=%d --gres=gpu:%d",
        resources->memory,
        time_str,
        resources->cpus,
        resources->gpus
    );
    free(time_str);

    // Build submission command
    int written = snprintf(
        cmd,
        MAX_SLURM_COMMAND_LENGTH,
        "sbatch --parsable -o %s -e %s %s --wrap='%s --call-packet %s --socket-base %s --output-file %s --output-form packet",
        output_filename,
        error_filename,
        resources_spec,
        nexus_path,
        call_packet_filename,
        socket_basename,
        result_cache_filename
    );

    RAISE_IF(written >= MAX_SLURM_COMMAND_LENGTH, "Command too long")

    // Submit job and capture output
    slurm = popen(cmd, "r");
    RAISE_IF(!slurm, "Failed to execute sbatch")

    // Parse job ID from first line
    if (fscanf(slurm, "%u", &job_id) != 1) {
        pclose(slurm);
        RAISE("Failed to parse job ID from sbatch output");
    }

    pclose(slurm);
    return job_id;
}


// Before calling this function, every argument must be converted to a packet
//   * if the data is native, then it should be converted
uint8_t* remote_call(
    int midx,

    // The base socket name for the target pool (e.g., "pipe-r").
    // This is not the full socket path, since the parent and remote will be
    // using different socket files in different temporary directories. But the
    // basename generation will be conserved.
    const char* socket_basename,

    // path where args and results will be written
    const char* cache_path,

    // required system resources (mem, cpus, etc)
    const resources_t* resources,

    // voidstar for each argument
    const uint8_t** arg_packets,

    // number of arguments
    size_t nargs,
    ERRMSG
){
    ERROR_HANDLING_SETUP

    uint64_t seed = (uint64_t)midx;

    // Initialization of allocated data
    uint8_t* return_packet = NULL;
    uint64_t* arg_hashes = NULL;
    uint8_t** arg_voidstars = NULL;
    Schema** arg_schemas = NULL;
    char** cached_arg_filenames = NULL;
    uint8_t* call_packet = NULL;
    char* result_cache_filename = NULL;
    uint8_t** cached_arg_packets = NULL;
    char* call_packet_filename = NULL;
    char* output_filename = NULL;
    char* error_filename = NULL;

    // Initializations
    size_t call_packet_size = 0;
    uint64_t call_packet_hash_code = 0;
    char output_ext[] = ".out";
    char error_ext[] = ".err";
    char call_ext[] = "-call.dat";

    uint32_t pid = 0;
    size_t return_packet_size = 0;
    char* failure = NULL;

    // Initialize function hash
    //
    // The function hash determines the output file name on the remote node and is
    // used to determine if this computation has already been run. The function
    // hash **should** be unique to a function and its inputs.
    //
    // TODO: Actually hash the function code, not just the manifold id.
    uint64_t function_hash = mix(seed, DEFAULT_XXHASH_SEED);

    // Collect the hash of the voidstar data in every argument packet. This is
    // independent of language, does not consider the header or metadata, and is
    // unaffected by representation in the shared memory pool.
    arg_hashes = (uint64_t*)calloc(nargs, sizeof(uint64_t));

    // Collect the voidstar data for each argument
    arg_voidstars = (uint8_t**)calloc(nargs, sizeof(uint8_t*));

    // Collect the schema for each argument. This schema must be written into
    // the packet metadata (which will be done automatically by the packet
    // creators provided in this library.
    arg_schemas = (Schema**)calloc(nargs, sizeof(Schema*));

    // It would be rather better if I automatically hashed every packet at
    // creation time and stored the hash in the packet metadata. For file
    // sources, then I would need to hash the file and store the hash time.
    // Then I could avoid unpacking the data.
    for(size_t i = 0; i < nargs; i++){

        // direct pointer to the packet string (do not free)
        char* arg_schema_str = TRY_GOTO(read_schema_from_packet_meta, arg_packets[i]);

        arg_schemas[i] = TRY_GOTO(parse_schema, (const char**)&arg_schema_str);

        // get the raw data stored in the packet (after the header and metadata)
        // possibly heavy memory
        arg_voidstars[i] = TRY_GOTO(get_morloc_data_packet_value, arg_packets[i], arg_schemas[i]);

        arg_hashes[i] = TRY_GOTO(hash_voidstar, arg_voidstars[i], arg_schemas[i], DEFAULT_XXHASH_SEED);

        // update function hash with ith argument hash
        function_hash = mix(function_hash, arg_hashes[i]);
    }

    TRY_GOTO(mkdir_p, cache_path)

    call_packet = NULL;
    result_cache_filename = check_cache_packet(function_hash, cache_path, &CHILD_ERRMSG);
    CHILD_ERRMSG = NULL; // ignore error
                         //
    // If a cached result already exists, return it
    if(result_cache_filename != NULL){
        // return result is cached, so load the cache and go
        return_packet = TRY_GOTO(get_cache_packet, function_hash, cache_path);
        goto end;
    } else {
        result_cache_filename = TRY_GOTO(make_cache_filename, function_hash, cache_path);
    }

    // return result is not cached, so we need to run
    cached_arg_filenames = (char**)calloc(nargs, sizeof(char*));
    for(size_t i = 0; i < nargs; i++){
        cached_arg_filenames[i] = check_cache_packet(arg_hashes[i], cache_path, &CHILD_ERRMSG);
        if(cached_arg_filenames[i] == NULL){
            CHILD_ERRMSG = NULL; // ignore error, if it failed, remake the cache
            cached_arg_filenames[i] = TRY_GOTO(put_cache_packet, arg_voidstars[i], arg_schemas[i], arg_hashes[i], cache_path);
        }
    }

    // read the cached argument packets (these will be small since they contain
    // only a wrapper around the MessagePack file names)
    cached_arg_packets = (uint8_t**)calloc(nargs, sizeof(uint8_t*));
    for(size_t i = 0; i < nargs; i++){
        size_t file_size = 0;
        cached_arg_packets[i] = TRY_GOTO(read_binary_file, cached_arg_filenames[i], &file_size);
    }

    // write the call packet to cache
    call_packet = TRY_GOTO(
        make_morloc_remote_call_packet,
        (uint32_t)midx,
        (const uint8_t**)cached_arg_packets,
        nargs
    );

    call_packet_size = TRY_GOTO(morloc_packet_size, call_packet);

    // Note that this is not the same as the result hash, the remote compute
    // node will load this packet as a call to run the job and will then write
    // the results to the result hash cache.
    call_packet_hash_code = XXH64(call_packet, call_packet_size, DEFAULT_XXHASH_SEED);

    call_packet_filename = TRY_GOTO(make_cache_filename_ext, call_packet_hash_code, cache_path, call_ext);

    // Write packet the call to disk, this will be sent the worker daemon
    TRY_GOTO(write_atomic, call_packet_filename, call_packet, call_packet_size)

    output_filename = TRY_GOTO(make_cache_filename_ext, function_hash, cache_path, output_ext);
    error_filename = TRY_GOTO(make_cache_filename_ext, function_hash, cache_path, error_ext);

    // submit slurm call, save process ID for watching and killing, if needed
    pid = TRY_GOTO(
        submit_morloc_slurm_job,
        "./nexus", // TODO: need a non-hard-coded path here
        socket_basename,
        call_packet_filename,
        result_cache_filename,
        output_filename,
        error_filename,
        resources
    );

    // Wait forever. Since this is a remote job, it is assumed that it will be
    // rather heavy so the 1 second loop is probably fine.
    while(!slurm_job_is_complete(pid)) {
        sleep(1);
    }

    return_packet_size = 0;
    return_packet = TRY_GOTO(read_binary_file, result_cache_filename, &return_packet_size);

    failure = TRY_GOTO(get_morloc_data_packet_error_message, return_packet)
    if(failure != NULL){
        fprintf(stderr, "Failed, deleting result %s\n", result_cache_filename);
        unlink(result_cache_filename);
    }

end:
    for(size_t i = 0; i < nargs; i++){
        if(arg_schemas != NULL){
            FREE(arg_schemas[i])
        }
        if(cached_arg_filenames != NULL){
            FREE(cached_arg_filenames[i])
        }
        if(cached_arg_packets != NULL){
            FREE(cached_arg_packets[i])
        }
    }

    FREE(arg_hashes)
    FREE(arg_voidstars)
    FREE(arg_schemas)
    FREE(result_cache_filename)
    FREE(call_packet_filename)
    FREE(output_filename)
    FREE(error_filename)
    FREE(cached_arg_filenames)
    FREE(cached_arg_packets)

    return return_packet;
}

// }}} end slurm support

// #endif // ending SLURM_SUPPORT

#endif // ending __MORLOC_H__
