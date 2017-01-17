#ifndef __COUPLET_H__
#define __COUPLET_H__

#include <stdlib.h>

typedef struct Couplet{
    struct W* lhs;
    struct W* rhs;
    char op;
} Couplet;

Couplet* couplet_new(struct W* lhs, struct W* rhs, char op);

#endif
