#ifndef __TYPE_HOMOSETLIST_H__
#define __TYPE_HOMOSETLIST_H__

#include "type_HomoSet.h"
#include "type_ManifoldList.h"


typedef struct HomoSetList{
    HomoSet* set;
    struct HomoSetList* next;
    struct HomoSetList* prev;
} HomoSetList;

HomoSetList* append_HomoSetList(HomoSetList* hsl, HomoSet* hs);
HomoSetList* create_HomoSetList(ManifoldList*);

void print_HomoSetList(HomoSetList* hsl);

#endif
