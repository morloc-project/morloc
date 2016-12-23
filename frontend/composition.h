#ifndef __COMPOSITION_H__
#define __COMPOSITION_H__

#include "manifold.h"
#include "list.h"

#include <stdlib.h>

typedef enum {
    C_UNDEFINED,
    C_VARIABLE,
    C_POSITIONAL,
    C_GROUP,
    C_CONDITIONAL,
    C_NEST
} ComposonType;

typedef struct Composon{
    ComposonType type;
    Manifold* manifold;
    union {
        char* name;
        List* nest;
    } value;
} Composon;

Composon* composon_new(ComposonType type);

void print_paths(NamedList* l, char* cmd);

List* composition_new(Composon* c);

List* composition_new_nested(List* comp);

List* composition_add_down(List* la, List* lb);

List* composition_compose(List* a, List* b);

#endif
