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


void resolve_derefs(Ws* ws){
    // set nargs within the derefs
    ws_rcmod(ws, ws_recurse_most, w_is_deref, _resolve_one_deref);
    // link manifolds calling derefs
    // (what if you have multiple compositions?)
    // e.g.
    // map2 . &( foo . $1 $2 ) &( bar . $1 $2 ) x y
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

    // I know ...
    W* tmp = w_new(C_NEST, ws_new(w_new(C_NEST, g_ws(w))));
    Ws* comp_out = composon_outputs(tmp);

    if(ws_length(comp_out) != 1){
        fprintf(stderr, "Multiple outputs in compositions is undefined\n");
        return;
    }

    W* output = comp_out->head;

    // Find the highest ARGREF, e.g. 3 in `(g . $3 f . $1 $2 )`
    W* margs = ws_scrap(
        g_ws(w),
        NULL,
        ws_recurse_most,
        w_is_argref,
        _highest_argref
    );

    // pass ARGREF onto all descendents of this DEREF
    ws_modcrawl(
        g_ws(w),
        margs,
        ws_recurse_most,
        _w_is_manifold,
        _set_nargs
    );

    // get derefs 
    // get inputs
    // count inputs
    // set all descendent manifolds to this number of inputs

    // for each deref
    //   create new manifold
    //   assert name is a T_PATH name
    //   clone the T_PATH (recursing through the inputs)
    //   find all inputs manifolds
    //   find all positionals in the inputs
    //   final all outputs (assert there is only one)
    //   define type (if possible)
    //   convert each manifold in the path to an open manifold (D_MANIFOLD)
    //
    // On backend, add 
    //
    // @path
    // Main :: map2 . &foo m n
    // foo :: f . g . a b
    //
    // # This might compile into something like:
    // map2 <- function(m,n) { for(mi in m){ for(ni in n){ d0(mi, ni) } } }
    // d0 <- function(x,y) { f( d1(x,y) ) }
    // d1 <- function(x,y) { g( d2(x,y), d3(x,y) ) }
    // d2 <- function(x,y) { a( x ) }
    // d3 <- function(x,y) { b( y ) }
    //
    // # The LIL might be something like:
    //
    // EMIT m1 R
    // FUNC m1 map2
    // INPF m1 0 d0
    // INPM m1 1 mx  # whatever 'm' is
    // INPM m1 2 my  # whatever 'n' is
    //
    // EMIT d0 R
    // FUNC d0 f
    // MARG d0 0 x
    // MARG d0 1 y
    // INPM d0 0 d1
    //
    // EMIT d1 R
    // FUNC d1 g
    // MARG d1 0 x
    // MARG d1 1 y
    // INPM d1 0 d2
    // INPM d1 1 d2
    //
    // EMIT d2 R
    // FUNC d2 a
    // MARG d2 0 x
    // MARG d2 1 y
    // FARG d2 0 x
    // FARG d2 0 y
    //
    // EMIT d3 R
    // FUNC d3 a
    // MARG d3 0 x
    // MARG d3 1 y
    // FARG d3 0 x
    // FARG d3 0 y
}
