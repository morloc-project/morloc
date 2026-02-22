#ifndef __MORLOC_JSON_H__
#define __MORLOC_JSON_H__

#include "morloc.h"

uint8_t* read_json_with_schema(uint8_t* voidstar, char* json_data, const Schema* schema, ERRMSG);

// ---- JSON writer ----

typedef struct json_buf_s {
    char* data;
    size_t len;
    size_t cap;
    bool needs_comma;  // tracks whether next value needs a preceding comma
} json_buf_t;

json_buf_t* json_buf_new(void);
void json_buf_free(json_buf_t* buf);
char* json_buf_finish(json_buf_t* buf);  // returns owned string, frees buf struct

void json_write_obj_start(json_buf_t* buf);
void json_write_obj_end(json_buf_t* buf);
void json_write_arr_start(json_buf_t* buf);
void json_write_arr_end(json_buf_t* buf);
void json_write_key(json_buf_t* buf, const char* key);
void json_write_string(json_buf_t* buf, const char* val);
void json_write_int(json_buf_t* buf, int val);
void json_write_uint(json_buf_t* buf, unsigned int val);
void json_write_bool(json_buf_t* buf, bool val);
void json_write_null(json_buf_t* buf);
void json_write_raw(json_buf_t* buf, const char* raw);

// Serialize voidstar data to a JSON string (caller must free result)
char* voidstar_to_json_string(const void* voidstar, const Schema* schema, ERRMSG);

#endif // __MORLOC_JSON_H__
