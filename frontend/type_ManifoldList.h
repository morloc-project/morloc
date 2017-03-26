#ifndef __TYPE_MANIFOLDLIST_H__
#define __TYPE_MANIFOLDLIST_H__

#include "ws_access.h"

typedef struct ManifoldList{
    size_t size;
    Manifold ** list;
} ManifoldList;

ManifoldList* create_ManifoldList(Ws* ws_top);

void print_ManifoldList(ManifoldList*);

size_t get_max_uid(ManifoldList* ml);

#endif
