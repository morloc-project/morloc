#include "build.h"

void _link_inputs(Ws* ws_top);
void _link_couplets(Ws* ws_top);
bool _resolve_grprefs_r(Ws* global, Ws* current);
void _resolve_grprefs(Ws* ws_top);


// Given the couplet {Label, Manifold}, transfer the name from Label to
// Manifold->function IFF it is not already defined.
void _set_default_manifold_function(const W* cm){
    Manifold* m = g_manifold(g_rhs(cm));
    if(!m->function){
        m->function = strdup(g_label(g_lhs(cm))->name);
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

bool _manifold_modifier(const W* w){
    switch(w->cls){
        case T_EFFECT:
        case T_CACHE:
        case T_CHECK:
        case T_OPEN:
        case T_PACK:
        case T_PASS:
        case T_FAIL:
        case T_DOC:
            return true;         
        default:
            return false;
    } 
}

// add the modifier stored in p (rhs of couplet) to w
// if:
//   1. the p->lhs contains only one name
//   2. the name matches the name of w
void _add_modifier(const W* w, const W* p){
    if(!p || w->cls != C_MANIFOLD) return;
    Manifold* m = g_manifold(g_rhs(w));
    W* rhs = g_rhs(p);
    switch(p->cls){
        case T_EFFECT:
            m->effect = ws_add_val(m->effect, P_STRING, g_string(rhs));
            break;
        case T_CACHE:
            m->cache = ws_add_val(m->cache, P_STRING, g_string(rhs));
            break;
        case T_CHECK:
            m->check = ws_add_val(m->check, P_STRING, g_string(rhs));
            break;
        case T_OPEN:
            m->open = ws_add_val(m->open, P_STRING, g_string(rhs));
            break;
        case T_PACK:
            m->pack = ws_add_val(m->pack, P_STRING, g_string(rhs));
            break;
        case T_PASS:
            m->pass = ws_add_val(m->pass, P_STRING, g_string(rhs));
            break;
        case T_FAIL:
            m->fail = ws_add_val(m->fail, P_STRING, g_string(rhs));
            break;
        case T_DOC:
            m->doc = ws_add_val(m->doc, P_STRING, g_string(rhs));
            break;
        default:
            break;
            fprintf(
                stderr, "Illegal p (%s) in %s:%d\n",
                w_class_str(p->cls), __func__, __LINE__
            );
    }
}

bool _basename_match(const W* w, const W* p){
    bool result = false;
    if(w->cls == C_MANIFOLD){
        Ws* pws = g_ws(g_lhs(p));
        result =
            ws_length(pws) == 1 &&
            label_cmp(g_label(pws->head), g_label(g_lhs(w)));
    }
    return result;
}

void _mod_pathwise(Ws* ws_top, const W* p){
    ws_prmod(ws_top, p, ws_recurse_path, _basename_match, _add_modifier, w_nextval_ifpath);
}

void _link_couplets(Ws* ws_top){

    Ws* cs = ws_rfilter(ws_top, ws_recurse_most, _manifold_modifier);

    cs = ws_map_split(cs, ws_split_couplet);

    ws_map_pmod(ws_top, cs, _mod_pathwise);

}


void _resolve_one_grpref(W* e_ref, const Ws* global){

    // TODO complete this function I need to write a ws_clone function

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

Ws* _recurse_path(const W* w){
    switch(w->cls){
        case T_PATH:
        case C_COMPOSON:
        case C_NEST:
            return g_ws(w);
        default:
            return NULL;
    }
}

bool _is_grpref(const W* c){
    return c->cls == C_GRPREF;
}

/* Requires input of both a global and current table. The global one is the top
 * level symbol table where all paths should be searched without recursion. The
 * current table is where group references should be sought.*/
void _resolve_grprefs(Ws* ws_top){
    ws_ref_rmod(
        ws_top, // current list over which to recurse
        ws_top, // global list (passed into _resolve_one_grpref
        _recurse_path, // recursion rule
        _is_grpref, // criterion for calling resolve_one_grpref
        _resolve_one_grpref // main operation
    );
}

void build_manifolds(Ws* ws_top){
    /* _resolve_grprefs(ws_top); */

    ws_filter_mod(ws_top, _get_manifolds, _set_default_manifold_function);

    _link_couplets(ws_top);
    _link_inputs(ws_top);
}
