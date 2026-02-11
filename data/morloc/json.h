#ifndef __MORLOC_JSON_H__
#define __MORLOC_JSON_H__

#include "morloc.h"

uint8_t* read_json_with_schema(uint8_t* voidstar, char* json_data, const Schema* schema, ERRMSG);

#endif // __MORLOC_JSON_H__
