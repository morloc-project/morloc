#include "build_grp.h"

void _resolve_grprefs_r(Ws* current, const Ws* global);
bool _is_grpref(const W* c);
void _resolve_one_grpref(W* e_ref, const Ws* global);
bool _matches_path(const W* w, const W* p);


void resolve_grprefs(Ws* ws){
    _resolve_grprefs_r(ws, ws);
}

/* Requires input of both a global and current table. The global one is the top
 * level symbol table where all paths should be searched without recursion. The
 * current table is where group references should be sought.*/
void _resolve_grprefs_r(Ws* current, const Ws* global){
    ws_ref_rmod(
        current, // current list over which to recurse
        global,  // global list (passed into _resolve_one_grpref
        ws_recurse_most, // recursion rule
        _is_grpref, // criterion for calling resolve_one_grpref
        _resolve_one_grpref // main operation
    );
}

bool _is_grpref(const W* c){
    return c->cls == C_GRPREF;
}

void _resolve_one_grpref(W* e_ref, const Ws* global){

    Ws* path = ws_pfilter(global, e_ref, _matches_path);

    if(!path){
        fprintf(stderr, "ERROR: group reference could not be resolved -- path not found\n");
        return;
    }
    if(ws_length(path) > 1){
        fprintf(stderr, "ERROR: group reference could not be resolved -- ambiguous paths\n"); 
        return;
    }

    force_set_couplet(e_ref, T_PATH, g_couplet(path->head));

    w_clone_value(e_ref);

    _resolve_grprefs_r(g_ws(g_rhs(e_ref)), global);

}

bool _matches_path(const W* w, const W* p){
    return
        w->cls == T_PATH &&
        p->cls == C_GRPREF &&
        strcmp(g_label(g_lhs(w))->name, g_string(p)) == 0;
}
