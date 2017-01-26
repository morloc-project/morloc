#include "build_path.h"

void _resolve_grprefs_r(Ws* current, Ws* global);
void _resolve_one_grpref(W* e_ref, Ws* global);
bool _matches_path(W* w, W* p);

void _resolve_one_deref(W* w);

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
        warn("ERROR: group reference could not be resolved -- path not found\n");
        return;
    }
    if(ws_length(path) > 1){
        warn("ERROR: group reference could not be resolved -- ambiguous paths\n"); 
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


void resolve_derefs(Ws* ws){
    // set nargs within the derefs
    ws_rcmod(ws, ws_recurse_most, w_is_deref, _resolve_one_deref);
}

W* _highest_argref(W* w, W* p){
    return (!p || g_string(w)[0] > g_string(p)[0]) ? w : p;
}

void _set_nargs(W* w, W* p){
    sscanf(g_string(p), "%d", &g_manifold(g_rhs(w))->nargs);
}

bool _w_is_manifold(W* w, W* p){
    return w_is_manifold(w);
}

void _resolve_one_deref(W* w){
    // Find the highest ARGREF, e.g. 3 in `(g . $3 f . $1 $2 )`
    W* margs = ws_scrap(
        g_ws(w),
        NULL,
        ws_recurse_most,
        w_is_argref,
        _highest_argref
    );

    if(margs){
        // pass ARGREF onto all descendents of this DEREF
        ws_modcrawl(
            g_ws(w),
            margs,
            ws_recurse_most,
            _w_is_manifold,
            _set_nargs
        );
    }
}


// NOTE: this propagates the number of manifold arguments to the head manifold
// in the given composition. It does not propagate into the rest of the
// composition. This job is performed when input/oututs are linked.
void _copy_nargs(W* a, W* b){
    Manifold* ma = g_manifold(g_rhs(a));
    Manifold* mb = g_manifold(b);
    ma->nargs = mb->nargs;
}
bool _has_args(W* a, W* b){
    return g_manifold(b)->nargs != 0;
}
void _propagate_one_nargs(W* w){
    Manifold* m = g_manifold(g_rhs(w));

    ws_cap(m->check, g_rhs(w), _has_args, _copy_nargs);

    ws_cap(m->h0, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h1, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h2, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h3, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h4, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h5, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h6, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h7, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h8, g_rhs(w), _has_args, _copy_nargs);
    ws_cap(m->h9, g_rhs(w), _has_args, _copy_nargs);

    ws_cap(m->fail, g_rhs(w), _has_args, _copy_nargs);
}
void propagate_nargs(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _propagate_one_nargs
    );
}
