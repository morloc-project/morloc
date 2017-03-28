#ifndef __MANIFOLD_H__
#define __MANIFOLD_H__

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct Manifold {
    size_t uid;
    char* function;
    char* lang;
    struct Ws* h0;     // Couplet<char*>
    struct Ws* h1;     // "
    struct Ws* h2;     // "
    struct Ws* h3;     // "
    struct Ws* h4;     // "
    struct Ws* h5;     // "
    struct Ws* h6;     // "
    struct Ws* h7;     // "
    struct Ws* h8;     // "
    struct Ws* h9;     // "
    struct Ws* cache;  // "
    struct Ws* assert; // "
    struct Ws* fail;   // "
    struct Ws* doc;    // "
    struct Ws* inputs; // Couplet<Manifold*>
    struct Ws* args;   // Couplet<P_STRING,Ws<P_STRING>>
    struct Ws* type;   // P_STRING | P_WS
    int nargs;         // args passed to manifold and onto children
    bool as_function;
} Manifold;

Manifold* manifold_new();

// Creates a copy of m with a new uid
Manifold* manifold_clone(Manifold* m);

#endif
