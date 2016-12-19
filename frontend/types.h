#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <stdio.h>

typedef struct Couplet{
    char* name;
    char* value;
} Couplet;

typedef struct CoupletStack{
    Couplet* value;
    struct CoupletStack *next;
} CoupletStack;

void put_Couplet(CoupletStack* cs, char* name, char* value);
Couplet* pop_Couplet(CoupletStack* cs);
CoupletStack* new_CoupletStack();

#endif
