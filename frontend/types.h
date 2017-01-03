#ifndef __TYPES_H__
#define __TYPES_H__

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct Couplet{
    struct W* lhs;
    struct W* rhs;
} Couplet;

typedef struct Manifold {
    int uid;
    char* function;
    struct Ws* effect; // Couplet<char*>
    struct Ws* cache;  // Couplet<char*>
    struct Ws* check;
    struct Ws* open;
    struct Ws* pack;
    struct Ws* pass;
    struct Ws* fail;
    struct Ws* doc;
    struct Ws* inputs;  // Couplet<Manifold*>
} Manifold;

typedef struct Label{
    char* name;
    char* label;
} Label;

Manifold* manifold_new();

Label* label_new();

Label* label_new_set(char* name, char* label);

Couplet* couplet_new(struct W* lhs, struct W* rhs);

bool label_cmp(Label* a, Label* b);

#endif
