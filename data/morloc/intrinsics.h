#ifndef __MORLOC_INTRINSICS_H__
#define __MORLOC_INTRINSICS_H__

#include "schema.h"
#include "macros.h"
#include "cli.h"

#ifdef __cplusplus
extern "C" {
#endif

// Save voidstar data to file as raw msgpack
int mlc_save(const absptr_t data, const Schema* schema, const char* path, ERRMSG);

// Save voidstar data to file as plain JSON text
int mlc_save_json(const absptr_t data, const Schema* schema, const char* path, ERRMSG);

// Save voidstar data to file as a morloc data packet in flat binary format
int mlc_save_voidstar(const absptr_t data, const Schema* schema, const char* path, ERRMSG);

// Load data from file, auto-detecting format (morloc packet, raw msgpack, or JSON)
// Returns NULL if file doesn't exist (not an error), sets errmsg on parse failure
void* mlc_load(const char* path, const Schema* schema, ERRMSG);

// Hash voidstar data, returns 16-char hex string (caller frees)
char* mlc_hash(const absptr_t data, const Schema* schema, ERRMSG);

// Serialize voidstar data to a JSON string (caller frees)
char* mlc_show(const absptr_t data, const Schema* schema, ERRMSG);

// Deserialize a JSON string to voidstar data (returns NULL on parse failure)
void* mlc_read(const char* json_str, const Schema* schema, ERRMSG);

// Write flattened voidstar binary to an fd (used by @save voidstar variant)
relptr_t write_voidstar_binary(int fd, const void* data, const Schema* schema, ERRMSG);

#ifdef __cplusplus
}
#endif

#endif
