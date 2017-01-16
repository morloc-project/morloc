#include "build_path.h"

void _resolve_grprefs_r(Ws* current, Ws* global);
void _resolve_one_grpref(W* e_ref, Ws* global);
bool _matches_path(W* w, W* p);

void resolve_grprefs(Ws* ws){
    _resolve_grprefs_r(ws, ws);
}

/* Requires input of both a global and current table. The global one is the top
 * level symbol table where all paths should be searched without recursion. The
 * current table is where group references should be sought.*/
void _resolve_grprefs_r(Ws* current, Ws* global){
    ws_ref_rmod(
        current, // current list over which to recurse
        global,  // global list (passed into _resolve_one_grpref
        ws_recurse_most, // recursion rule
        w_is_grpref, // criterion for calling resolve_one_grpref
        _resolve_one_grpref // main operation
    );
}

void _resolve_one_grpref(W* e_ref, Ws* global){

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

bool _matches_path(W* w, W* p){
    return
        w->cls == T_PATH &&
        p->cls == C_GRPREF &&
        strcmp(g_label(g_lhs(w))->name, g_string(p)) == 0;
}


void resolve_refered(W* man, W* ref){
    if(ref->cls == C_REFER){
        s_couplet(ref, g_couplet(man));
    }
}
bool w_is_matching_manifold(W* w, W* ref){
    bool result = false;
    if(w->cls == C_MANIFOLD){
        result = w_equal_lhs(w, ref);
    }
    return result;
}
void seek_referenced_manifold(W* ref, Ws* top){
    ws_modcrawl(
        top,
        ref,
        ws_recurse_most,
        w_is_matching_manifold,
        resolve_refered
    );
}
void resolve_refers(Ws* ws){
    ws_ref_rmod(
        ws,
        ws, 
        ws_recurse_most,
        w_is_refer,
        seek_referenced_manifold
    );
}
