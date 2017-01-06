#include "type.h"

void set_default_types(const Ws* ws){
    /* STUB */
}

Ws* infertype(const Ws* ws){
    /* STUB */
    return NULL;
}

void _typecheck(W* w){
    Manifold* m = g_manifold(g_rhs(w));
    if(!m->cache){
        fprintf(stderr, "no type\n");
    } else {
        fprintf(stderr, "OK\n");
    }
}
void typecheck(const Ws* ws){
    ws_rcmod(ws, ws_recurse_composition, w_is_manifold, _typecheck);
}
