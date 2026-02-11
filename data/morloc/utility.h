#ifndef __MORLOC_UTILITY_H__
#define __MORLOC_UTILITY_H__

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <dirent.h> // used in delete_directory
#include <sys/stat.h>
#include <unistd.h>

#include "macros.h"

#ifdef __cplusplus
extern "C" {
#endif

void hex(const void *ptr, size_t size);
bool file_exists(const char *filename);
int mkdir_p(const char *path, ERRMSG);
void delete_directory(const char* path);
bool has_suffix(const char* x, const char* suffix);
char* dirname(char* path);
int write_atomic(const char* filename, const uint8_t* data, size_t size, ERRMSG);
int print_binary(const char *buf, size_t count, ERRMSG);
uint8_t* read_binary_fd(FILE* file, size_t* file_size, ERRMSG);
uint8_t* read_binary_file(const char* filename, size_t* file_size, ERRMSG);

#ifdef __cplusplus
}
#endif

#endif // __MORLOC_UTILITY_H__
