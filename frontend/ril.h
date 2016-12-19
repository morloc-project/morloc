#ifndef __RIL_H__
#define __RIL_H__

#include "types.h"

// Eventually this will be extended to cover all sections
typedef struct RatStack{
    CoupletStack* doc;
} RatStack;

RatStack* new_RatStack();

void print_RIL(RatStack* rs);

#endif
