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

Couplet* couplet_new(struct W* lhs, struct W* rhs);


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

Manifold* manifold_new();

// Creates a copy of m with a new uid
Manifold* manifold_clone(Manifold* m);


typedef struct Label{
    char* name;
    char* label;
} Label;

Label* label_new();

// creates a new label with copies of the original strings
Label* label_copy(Label*);

Label* label_new_set(char* name, char* label);

bool label_cmp(Label* a, Label* b);

#endif
