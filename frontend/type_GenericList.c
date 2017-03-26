#include "type_GenericList.h"

void print_GenericList(GenericList* gl){
    fprintf(stderr, "Generic List\n");
    for(size_t i = 0; i < gl->size; i++){
        if(gl->list[i]){
            fprintf(stderr, " - ");
            print_Generic(gl->list[i]);
        }
    }
}

void _r_load_generics(GenericList* gl, W* w, Manifold* m, int gmod){
    switch(w->cls){
        case FT_FUNCTION:
        case FT_TUPLE:
        case FT_ARRAY:
        {
            for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
                _r_load_generics(gl, wt, m, gmod);
            }
            break;
        }
        case FT_GENERIC:
        {
            // - The lhs holds the generic label (e.g. 'a')
            // - The rhs of a generic holds the inferred type it will be of type
            size_t gid = gmod + g_string(g_lhs(w))[0];
            if(gl->size <= gid){
                fprintf(stderr, "Invalid id at %s:%d\n", __func__, __LINE__);
            }
            gl->list[gid] = append_Generic(gl->list[gid], w, m);
            break;
        }
        case FT_ATOMIC:
        case C_POSITIONAL:
            // Always explicit types
            break;
        default:
            warn("Expected FT_* at (%s:%d), got %s", __func__, __LINE__, w_str(w)); 
            break;
    }
}

GenericList* create_GenericList(ManifoldList* ml){ 
    GenericList* gl = (GenericList*)malloc(sizeof(GenericList));
    gl->size = get_generic_size(get_max_uid(ml)+1);
    gl->list = (Generic**)calloc(gl->size, sizeof(Generic*));
    for(size_t i = 0; i < ml->size; i++){
        Manifold* m = ml->list[i]; 
        for(W* type = ws_head(m->type); type; type = type->next){
            _r_load_generics(gl, type, m, get_generic_id_offset(m->uid));
        }
    }
    return gl; 
}

Generic* access_GenericList(int gid){ return NULL; }
