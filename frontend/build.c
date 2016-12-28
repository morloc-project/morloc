#include "build.h"

// link all top level elements in c_{i+1} as inputs to c_i
void link_inputs(Table* t_top){
    Table* t_path = table_recursive_get_type(t_top, T_PATH);
    t_path = table_join(t_path, table_recursive_get_type(t_top, C_NEST));
    for(Entry* e_path = t_path->head; e_path; e_path = e_path->next){
        for(Entry* e_com = e_path->value.table->head; e_com; e_com = e_com->next){
            Table* outputs = table_composon_outputs(e_com->next);

            if(!outputs) continue;

            Table* inputs = table_composon_inputs(e_com);
            for(Entry* o = outputs->head; o; o = o->next){
                for(Entry* i = inputs->head; i; i = i->next){
                    i->value.manifold->inputs = table_add(i->value.manifold->inputs, o);
                }
            }
        }
    }
}

/* Mouse has only one couplet type: EFFECT. Rat has a bunch. So the switch
 * statements will be more populous. This function does the following: 
 *  1. Find all couplets of the given type
 *  2. For each couplet:
 *  3.   Find all manifolds in its path
 *  4.   For each manifold couple the given element
 */
void link_couplets(Table* t_top, TType type){
    Table* t_couplet = table_recursive_get_type(t_top, type);
    if(t_couplet && t_couplet->head){
        for(Entry* e = t_couplet->head; e; e = e->next){
            Table* t_man = NULL;
            switch(type){
                case T_EFFECT:
                    t_man = table_selection_get(t_top, e->value.effect->selection, C_MANIFOLD);
                    break;
                default:
                    fprintf(stderr, "ILLEGAL TYPE\n");
                    exit(EXIT_FAILURE);
            }
            if(!t_man){
                continue;
            }
            for(Entry* ee = t_man->head; ee; ee = ee->next){
                Manifold* m = ee->value.manifold;
                switch(type){
                    case T_EFFECT:
                        m->effect = e->value.effect->function;
                        break;
                    default:
                        fprintf(stderr, "ILLEGAL TYPE\n");
                        exit(EXIT_FAILURE);
                }
            }
        }
    }
}

void build_manifolds(Table* t_top){
    link_inputs(t_top);
    link_couplets(t_top, T_EFFECT);
}
