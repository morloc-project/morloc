#include "build.h"

void _link_inputs(Ws* ws_top);
void _link_couplets(Ws* ws_top);
bool _resolve_grprefs_r(Ws* global, Ws* current);
void _resolve_grprefs(Ws* ws_top);
void _resolve_one_grpref(Ws* global, W* e_ref);


// Given the couplet {Label, Manifold}, transfer the name from Label to
// Manifold->function IFF it is not already defined.
void _set_default_manifold_function(const W* cm){
    ws_assert_class(cm, C_MANIFOLD);
    W* lhs = cm->value.couplet->lhs;
    W* rhs = cm->value.couplet->rhs;
    Manifold* m = rhs->value.manifold;
    if(!m->function){
        m->function = strdup(lhs->value.label->name);
    }
}

// Find all {label,manifold} couplets
Ws* _get_manifolds(const Ws* ws){
    return ws_rfilter(ws, ws_recurse_most, w_is_manifold);
}

// link all top level elements in c_{i+1} as inputs to c_i
void _link_inputs(Ws* ws_top){
    /* Table* t_path = table_recursive_get_type(t_top, T_PATH);                                 */
    /* t_path = table_join(t_path, table_recursive_get_type(t_top, C_NEST));                    */
    /* for(Entry* e_path = t_path->head; e_path; e_path = e_path->next){                        */
    /*     for(Entry* e_com = e_path->value.table->head; e_com; e_com = e_com->next){           */
    /*         Table* outputs = table_composon_outputs(e_com->next);                            */
    /*                                                                                          */
    /*         if(!outputs) continue;                                                           */
    /*                                                                                          */
    /*         Table* inputs = table_composon_inputs(e_com);                                    */
    /*         for(Entry* o = outputs->head; o; o = o->next){                                   */
    /*             for(Entry* i = inputs->head; i; i = i->next){                                */
    /*                 if(i->type == C_MANIFOLD){                                               */
    /*                     i->value.manifold->inputs = table_add(i->value.manifold->inputs, o); */
    /*                 }                                                                        */
    /*             }                                                                            */
    /*         }                                                                                */
    /*     }                                                                                    */
    /* }                                                                                        */
}

void _link_couplets(Ws* ws_top){
    Ws* t_couplet = ws_flatten(ws_top, ws_recurse_most);

    if(!t_couplet) return;

    /* for(Entry* e = t_couplet->head; e; e = e->next){                                        */
    /*     Table* t_man = NULL;                                                                */
    /*     switch(type){                                                                       */
    /*         case T_EFFECT:                                                                  */
    /*             t_man = table_selection_get(t_top, e->value.effect->selection, C_MANIFOLD); */
    /*             break;                                                                      */
    /*         default:                                                                        */
    /*             fprintf(stderr, "ILLEGAL TYPE\n");                                          */
    /*             exit(EXIT_FAILURE);                                                         */
    /*     }                                                                                   */
    /*                                                                                         */
    /*     if(!t_man) continue;                                                                */
    /*                                                                                         */
    /*     for(Entry* ee = t_man->head; ee; ee = ee->next){                                    */
    /*         Manifold* m = ee->value.manifold;                                               */
    /*         switch(type){                                                                   */
    /*             case T_EFFECT:                                                              */
    /*                 m->effect = e->value.effect->function;                                  */
    /*                 break;                                                                  */
    /*             default:                                                                    */
    /*                 fprintf(stderr, "ILLEGAL TYPE\n");                                      */
    /*                 exit(EXIT_FAILURE);                                                     */
    /*         }                                                                               */
    /*     }                                                                                   */
    /* }                                                                                       */

}

void _resolve_one_grpref(Ws* global, W* e_ref){
    /* Id* id = id_new();                                                                    */
    /* id->name = strdup(e_ref->value.string);                                               */
    /* Table* t_path = table_get(global, id, T_PATH);                                        */
    /* if(!t_path){                                                                          */
    /*     fprintf(stderr, "ERROR: path '%s', not found\n", id->name);                       */
    /* }                                                                                     */
    /* if(t_path->head->next){                                                               */
    /*     fprintf(stderr, "ERROR: Ambiguous path, using first\n");                          */
    /* }                                                                                     */
    /* Table* resolved = table_clone(t_path->head->value.table);                             */
    /* if(resolved){                                                                         */
    /*     e_ref->type = T_PATH;                                                             */
    /*     e_ref->value.table = resolved;                                                    */
    /*     _resolve_grprefs_r(global, resolved);                                             */
    /* } else {                                                                              */
    /*     fprintf(stderr, "ERROR: group reference '%s' could not be resolved\n", id->name); */
    /* }                                                                                     */
}

/* Requires input of both a global and current table. The global one is the top
 * level symbol table where all paths should be searched without recursion. The
 * current table is where group references should be sought.*/
bool _resolve_grprefs_r(Ws* global, Ws* current){
    /*                                                                */
    /* if(!current) return false;                                     */
    /*                                                                */
    /* for(Entry* e_ref = current->head; e_ref; e_ref = e_ref->next){ */
    /*     switch(e_ref->type){                                       */
    /*         case T_PATH:                                           */
    /*         case C_COMPOSON:                                       */
    /*         case C_NEST:                                           */
    /*             _resolve_grprefs_r(global, e_ref->value.table);    */
    /*             break;                                             */
    /*         case C_GRPREF:                                         */
    /*             _resolve_one_grpref(global, e_ref);                */
    /*             break;                                             */
    /*         default:                                               */
    /*             break;                                             */
    /*     }                                                          */
    /* }                                                              */
    return true;
}

void _resolve_grprefs(Ws* ws_top){
    /* _resolve_grprefs_r(t_top, t_top); */
}

void build_manifolds(Ws* ws_top){
    _resolve_grprefs(ws_top);

    ws_filter_mod(ws_top, _get_manifolds, _set_default_manifold_function);

    _link_inputs(ws_top);
    _link_couplets(ws_top);
}
