#ifndef __RIL_H__
#define __RIL_H__

#include "types.h"

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
} RatStack;

void rewind_RatStack();

RatStack* new_RatStack();

void print_RIL(RatStack* rs);

#endif
