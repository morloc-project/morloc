#include "type_Generic.h"

Generic* append_Generic(Generic* g, W* type, Manifold* m){
    if(!g){
        g = (Generic*)malloc(sizeof(Generic));
        g->type = type;
        g->list = NULL;
    }
    g->list = ws_add_val(g->list, P_MANIFOLD, m);
    return g;
}

void print_Generic(Generic* g){
    fprintf(stderr, "%s :: ", type_str(g->type)); 
    for(W* w = ws_head(g->list); w; w = w->next){
        Manifold* m = g_manifold(w);
        fprintf(stderr, "m%lu(%s)", m->uid, m->function);
        if(w->next)
            fprintf(stderr, " | ");
    }
    fprintf(stderr, "\n");
}
