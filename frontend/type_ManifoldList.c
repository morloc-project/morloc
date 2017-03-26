#include "type_ManifoldList.h"

W* r_wws_add(W* m, W* ms){
    // This is an add function that does not copy the value It is important to
    // use reference semantics here, so that the changes I make in the type
    // inferrence data structure are reflected in the original.
    return wws_add_val(ms, P_MANIFOLD, g_manifold(g_rhs(m)));
}
ManifoldList* create_ManifoldList(Ws* ws_top){
    W* ms = ws_scrap(
        ws_top,
        NULL,
        ws_recurse_most,
        w_is_manifold,
        r_wws_add
    );

    ManifoldList* ml = (ManifoldList*)malloc(1 * sizeof(ManifoldList));
    ml->size = wws_length(ms);
    ml->list = (Manifold**)calloc(ml->size, sizeof(Manifold*));
    for(W* w = wws_head(ms); w; w = w->next){
        Manifold* m = g_manifold(w);
        if(m->uid < ml->size){
            ml->list[m->uid] = m;
        } else {
            fprintf(stderr, "Aww, shucks, that shouldn't have happened");
        }
    }

    return ml;
}

void print_ManifoldList(ManifoldList* ml){
    fprintf(stderr, "Manifold List\n");
    for(size_t i = 0; i < ml->size; i++){
        fprintf(stderr, " - ");
        manifold_print(ml->list[i]); 
    }
}

size_t get_max_uid(ManifoldList* ml){
    size_t id = 0;
    for(size_t i = 0; i < ml->size; i++){
        size_t this_id = ml->list[i]->uid; 
        id = this_id > id ? this_id : id;
    }
    return id;
}
