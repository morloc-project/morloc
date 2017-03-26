#include "type_HomoSet.h"

HomoSet* append_HomoSet(HomoSet* hs, W* type, Manifold* m){
    HomoSet* x = (HomoSet*)malloc(sizeof(HomoSet));
    x->type = type;
    x->mid = m ? m->uid : -1;
    x->next = NULL;
    x->prev = hs;
    if(hs)
        hs->next = x;
    return x;
}

void print_HomoSet(HomoSet* hs){
    while(hs->prev){ hs = hs->prev; }
    for(; hs; hs = hs->next){
        fprintf(stderr, "%s", type_str(hs->type));
        if(hs->next)
            fprintf(stderr, " | ");
    }
    fprintf(stderr, "\n");
}
