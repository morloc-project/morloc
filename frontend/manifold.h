#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

#include <stdlib.h>
#include <string.h>

typedef struct Manifold {
    int uid;
    char* function;
    char* lang;
    struct Ws* effect; // Couplet<char*>
    struct Ws* hook;   // "
    struct Ws* cache;  // "
    struct Ws* check;  // "
    struct Ws* open;   // "
    struct Ws* pack;   // "
    struct Ws* pass;   // "
    struct Ws* fail;   // "
    struct Ws* doc;    // "
    struct Ws* inputs; // Couplet<Manifold*>
    struct Ws* args;   // Couplet<P_STRING,Ws<P_STRING>>
    struct Ws* type;   // P_STRING | P_WS
} Manifold;

Manifold* manifold_new();

// Creates a copy of m with a new uid
Manifold* manifold_clone(Manifold* m);

#endif
