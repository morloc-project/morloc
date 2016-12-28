#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

#include <stdlib.h>
#include <string.h>

typedef struct Manifold {
    int uid; // an id unique to this manifold
    // --- elements ----------------------------------------------------------
    char* function;
    char* effect;
    struct Table* inputs;
} Manifold;

Manifold* manifold_new();

#endif
