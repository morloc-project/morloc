#ifndef __LIL_H__
#define __LIL_H__

#include "list.h"
#include "composition.h"

#include <stdio.h>

// Eventually this will be extended to cover all sections
typedef struct RatStack{
    NamedList* export;
    NamedList* doc;
    NamedList* alias;
    NamedList* cache;
    NamedList* pack;
    NamedList* open;
    NamedList* fail;
    NamedList* pass;
    NamedList* source;
    List* ontology;
    List* type;
    NamedList* path;
    NamedList* check;
    NamedList* effect;
} RatStack;

void ratstack_rewind();

RatStack* ratstack_new();

void print_RIL(RatStack* rs);

#endif
