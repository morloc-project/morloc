#ifndef __MORLOC_CACHE_H__
#define __MORLOC_CACHE_H__

#include "morloc.h"

#define DEFAULT_XXHASH_SEED 0

uint64_t mix(uint64_t a, uint64_t b);
uint64_t hash_voidstar(absptr_t data, const Schema* schema, uint64_t seed, ERRMSG);
char* make_cache_filename(uint64_t key, const char* cache_path, ERRMSG);
char* make_cache_filename_ext(uint64_t key, const char* cache_path, const char* ext, ERRMSG);

#endif // __MORLOC_CACHE_H__
