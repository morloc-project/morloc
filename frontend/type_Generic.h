#ifndef __TYPE_GENERIC_H__
#define __TYPE_GENERIC_H__

#include "ws_access.h"
#include "type_util.h"

typedef struct Generic{
    W* type;
    Ws* list;
} Generic;

Generic* append_Generic(Generic* g, W* type, Manifold* m);
void print_Generic(Generic* g);

#endif
