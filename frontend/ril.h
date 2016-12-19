#ifndef __RIL_H__
#define __RIL_H__

#include "types.h"

// Eventually this will be extended to cover all sections
typedef struct RatStack{
    CoupletStack* doc;
    CoupletStack* alias;
    CoupletStack* cache;
    CoupletStack* pack;
    CoupletStack* open;
    CoupletStack* fail;
    CoupletStack* pass;
} RatStack;

RatStack* new_RatStack();

void print_RIL(RatStack* rs);

#endif
