#ifndef __RIL_H__
#define __RIL_H__

#include "types.h"

// Eventually this will be extended to cover all sections
typedef struct RatStack{
    List* export;
    List* doc;
    List* alias;
    List* cache;
    List* pack;
    List* open;
    List* fail;
    List* pass;
    // SourceLL* source;
} RatStack;

void rewind_RatStack();

RatStack* new_RatStack();

void print_RIL(RatStack* rs);

#endif
