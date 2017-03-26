#include "type_HomoSetList.h"

/* HomoSetList{                  */
/*     HomoSet* set;             */
/*     struct HomoSetList* next; */
/*     struct HomoSetList* prev; */
/* }                             */
HomoSetList* append_HomoSetList(HomoSetList* hsl, HomoSet* hs){
    HomoSetList* x = (HomoSetList*)malloc(sizeof(HomoSetList));
    x->set = hs; 
    x->next = NULL;
    x->prev = hsl;
    if(hsl)
        hsl->next = x;
    return x;
}

HomoSetList* create_HomoSetList(ManifoldList* ml){
    HomoSetList* hsl = NULL;
    for(size_t i = 0; i < ml->size; i++){
        Manifold* m = ml->list[i];
        W* x = ws_head(m->inputs);
        W* b = ws_head(m->type);
        for(; x && b; x = x->next, b = b->next){
            W* a = x;
            if(x->cls == C_MANIFOLD){
                a = ws_last(g_manifold(g_rhs(x))->type);
            }
            HomoSet* hs = append_HomoSet(NULL, a, m);
            hs = append_HomoSet(hs, b, m);
            hsl = append_HomoSetList(hsl, hs);
        }
    } 
    return hsl;
}

void print_HomoSetList(HomoSetList* hsl){
    while(hsl->prev){ hsl = hsl->prev; }
    fprintf(stderr, "Homogenous Sets\n");
    for(; hsl; hsl = hsl->next){
        fprintf(stderr, " - ");
        print_HomoSet(hsl->set);
    }
}
