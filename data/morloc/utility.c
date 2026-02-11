#include "utility.h"

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

bool file_exists(const char *filename) {
    return access(filename, F_OK) == 0; // F_OK checks for existence
}

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

bool has_suffix(const char* x, const char* suffix){
    if( x == NULL || suffix == NULL){
        return false;
    }

    size_t x_len = strlen(x);
    size_t suffix_len = strlen(suffix);

    return (x_len >= suffix_len) &&
           strcmp(x + x_len - suffix_len, suffix) == 0;
}

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
