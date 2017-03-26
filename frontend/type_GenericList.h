#ifndef __TYPE_GENERICLIST_H__
#define __TYPE_GENERICLIST_H__

#include "type_Generic.h"
#include "type_ManifoldList.h"

typedef struct GenericList{
    size_t size;
    Generic ** list;
} GenericList;

GenericList* create_GenericList(ManifoldList* ml);

Generic* access_GenericList(int gid);

void print_GenericList(GenericList* gl);

#endif
