#ifndef __TYPE_UTIL_H__
#define __TYPE_UTIL_H__

#include "ws_access.h"
#include "type_macros.h"


bool cmp_type(char* a, char* b);

char* type_str(W* w);

bool type_is_io(W* w);
bool type_is_well(Ws* type);
bool type_is_pipe(Ws* type);
bool type_is_sink(Ws* type);
bool type_is_effectful(Ws* type);

size_t get_generic_id_from_uid(size_t uid, char c);
size_t get_generic_size(int max_uid);
size_t get_generic_id(W* w, char c);
size_t get_generic_id_offset(size_t uid);

#endif
