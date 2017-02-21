#include "ws_access.h"

Ws* ws_flatten( Ws* ws, Ws*(*recurse)(W*) ){
    return ws_rfilter(ws, recurse, w_keep_all);
}

Ws* ws_filter(Ws* ws, bool(*criterion)(W*)){
    return ws_rfilter(ws, ws_recurse_none, criterion);
}

// Find all {label,manifold} couplets
Ws* get_manifolds(Ws* ws){
    return ws_rfilter(ws, ws_recurse_most, w_is_manifold);
}

Ws* get_refers(Ws* ws){
    return ws_rfilter(ws, ws_recurse_most, w_is_refer);
}

Ws* get_tpaths(Ws* ws){
    return ws_rfilter(ws, ws_recurse_none, w_is_tpath);
}

bool _manifold_match(W* w, W* p){
    return w_is_manifold(w) && w_equal_lhs(w, p);
}
Ws* get_by_name(Ws* ws, W* p){
    W* p_;
    switch(p->cls){
        case K_LABEL:
        case K_LIST:
        case K_PATH:
        case K_NAME:
            p_ = w_new(P_COUPLET, couplet_new(p, NULL, '='));
            break;
        default:
            p_ = w_isolate(p);
            break;
    }
    return ws_prfilter(
        ws,
        p_,
        ws_recurse_path,
        _manifold_match,
        w_nextval_ifpath
    );
}

bool w_is_lang     ( W* w ){ return w ? w->cls == T_LANG     : false; }
bool w_is_type     ( W* w ){ return w ? w->cls == T_TYPE     : false; }
bool w_is_grpref   ( W* w ){ return w ? w->cls == C_GRPREF   : false; }
bool w_is_argref   ( W* w ){ return w ? w->cls == C_ARGREF   : false; }
bool w_is_refer    ( W* w ){ return w ? w->cls == C_REFER    : false; }
bool w_is_deref    ( W* w ){ return w ? w->cls == C_DEREF    : false; }
bool w_is_tpath    ( W* w ){ return w ? w->cls == T_PATH     : false; }
bool w_is_label    ( W* w ){ return w ? w->cls == K_LABEL    : false; }
bool w_is_manifold ( W* w ){ return w ? w->cls == C_MANIFOLD : false; }
bool w_is_export   ( W* w ){ return w ? w->cls == T_EXPORT   : false; }
bool w_is_source   ( W* w ){ return w ? w->cls == T_SOURCE   : false; }
bool w_is_composon ( W* w ){ return w ? w->cls == C_COMPOSON : false; }

bool w_is_ptype ( W* w ){
    switch(w->cls){
        case FT_FUNCTION:
        case FT_ATOMIC:
        case FT_ARRAY:
        case FT_TUPLE:
            return true;
        default:
            return false;
    }
}

bool w_is_recursive(W* w){ return w ? get_value_type(w->cls) == V_WS : false; }

bool w_is_named_composition(W* w){
    if(!w) return false;
    switch(w->cls){
        case T_PATH:
        case T_H0:
        case T_H1:
        case T_H2:
        case T_H3:
        case T_H4:
        case T_H5:
        case T_H6:
        case T_H7:
        case T_H8:
        case T_H9:
        case T_CHECK:
        case T_FAIL:
            return true;
        default:
            return false;
    }
}

Ws* ws_split_couplet(W* c){
    if(!c || !g_couplet(c)) return NULL;
    Ws* result = NULL;
    W* paths = g_lhs(c);
    switch(paths->cls){
        case K_LIST:
            {
                for(W* p = g_ws(paths)->head; p; p = p->next){
                    W* nc = w_isolate(c);
                    // This section has caused some tricky bugs
                    //
                    // I do not want to change the input W 'c'. So I need to
                    // copy 'c'. I need to alter the left-hand side (the selection)
                    // in the copy, but not in the original, so I need to clone
                    // the left-hand side.
                    //
                    // Cloning creates new things, manifolds gain new uids, for
                    // instance. This function should NOT alter the right-hand
                    // sides. So I must not clone both sides, e.g. this is not
                    // OK: `w_clone_value(nc)`

                    Couplet* cnew = couplet_new(p, g_rhs(c), g_couplet(c)->op);

                    s_couplet(nc, cnew);

                    result = ws_add(result, nc);
                }
            }
            break;
        case K_PATH:
        case K_LABEL:
        case K_NAME:
            result = ws_add(result, c); 
            break;
        default:
            warn("ERROR: invalid lhs type in couplet (%s:%d)", __func__, __LINE__);
            break;
    }
    return result;
}

// === nextval functions ============================================

W* w_nextval_always(W* w, W* p){ return p->next; }

W* w_nextval_never(W* w, W* p){ return p; }

/* p a modifier (e.g. check).
 * w a node into which we are recursing
 *
 * if w is a path, we need to pop the top level of p's lhs.
 */
W* w_nextval_ifpath(W* w, W* p) {
    W* next = p;
    if(w_is_named_composition(w) && ws_length(g_ws(g_lhs(p))) > 1){
        switch(g_lhs(p)->cls){
            case K_PATH:
                {
                // Gah, this is a real pain
                // 1) I mustn't modify the original p
                // 2) I mustn't clone the manifold (that changes the uid)
                next = w_isolate(p);
                s_couplet(next, couplet_copy(g_couplet(next)));                
                s_lhs(next, w_new(K_PATH, ws_tail(ws_clone(g_ws(g_lhs(next))))));
                s_rhs(next, w_copy(g_rhs(next)));
                }
                break;
            case K_LIST:
                next = NULL;
                // Not supported
                break;
            default:
                next = NULL;
                break;
        }
    }
    return next;
}

// === filter criteria ==============================================
// ------------------------------------------------------------------

bool w_keep_all(W* w){
    return true;
}

// === recursion rules ==============================================
// NOTE: recursion rules are splits
// ------------------------------------------------------------------

Ws* ws_recurse_most(W* w){
    if(!w) return NULL;
    Ws* rs = NULL;
    switch(get_value_type(w->cls)){
        case V_WS:
            rs = ws_add_val(rs, P_WS, g_ws(w));
            break;
        case V_COUPLET:
            {
                W* lhs = g_lhs(w);
                if(w_is_recursive(lhs)){
                    rs = ws_add_val(rs, P_WS, g_ws(lhs));
                }
                W* rhs = g_rhs(w);
                if(w_is_recursive(rhs)){
                    rs = ws_add_val(rs, P_WS, g_ws(rhs));
                }
            }
        default:
            break;
    }
    return rs;
}

Ws* ws_recurse_ws(W* w){
    if(!w) return NULL;
    Ws* rs = NULL;
    switch(get_value_type(w->cls)){
        case V_WS:
            rs = ws_add_val(rs, P_WS, g_ws(w));
            break;
        default:
            break;
    }
    return rs;
}

Ws* ws_recurse_section(W* w){
    Ws* rs = NULL;
    if(w->cls == T_SECTION){
        rs = ws_add_val(rs, P_WS, g_ws(g_rhs(w)));
    }
    return rs;
}

Ws* ws_recurse_none(W* w){
    return NULL;
}

Ws* ws_recurse_composition(W* w){
    if(!w) return NULL;
    Ws* rs = NULL;
    switch(w->cls){
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
            rs = ws_add_val(rs, C_NEST, g_ws(w));
            break;
        case T_SECTION:
            rs = ws_add_val(rs, P_WS, g_ws(g_rhs(w)));
            break;
        case T_PATH:
        case T_H0:
        case T_H1:
        case T_H2:
        case T_H3:
        case T_H4:
        case T_H5:
        case T_H6:
        case T_H7:
        case T_H8:
        case T_H9:
        case T_CHECK:
        case T_FAIL:
            rs = ws_add_val(rs, C_NEST, g_ws(g_rhs(w)));
            break;
        default:
            rs = NULL;
    }
    return rs;
}

Label* _ws_get_label_from_lhs(W* a){
    if(!a) return NULL;
    Label* label = NULL;
    switch(a->cls){
        case K_NAME:
            label = label_new_set(strdup(g_string(a)), NULL, NULL);
            break;
        case K_LABEL:
            label = g_label(a);
            break;
        case K_PATH:
            label = g_ws(a) ? g_label(g_ws(a)->head) : NULL;
            break;
        case K_LIST:
            label = NULL;
            // Recursion into K_LIST not supported
            break;
        default:
            label = NULL;
            warn("Illegal left hand side (%s:%d)", __func__, __LINE__);
            break;
    }
    return label;
}

bool w_equal_lhs(W* a, W* b){
    Label* a_label = _ws_get_label_from_lhs(g_lhs(a));
    Label* b_label = _ws_get_label_from_lhs(g_lhs(b));
    bool is_equal = label_cmp(a_label, b_label);
    return is_equal;
}

char* _ws_get_name(W* w){
    if(!w) return NULL;
    char* name = NULL;
    switch(get_value_type(w->cls)){
        case V_STRING:
            name = g_string(w);
            break;
        case V_COUPLET:
            name = _ws_get_name(g_lhs(w));
            break;
        case V_LABEL:
            name = g_label(w)->name;
            break;
        case V_MANIFOLD:
            name = g_manifold(w)->function;
            break;
        case V_SECTION:
            name = g_section(w)->name;
            break;
        case V_NONE:
            name = NULL;
            warn("Cannot get name from V_NONE (%s:%d)", __func__, __LINE__);
            break;
        case V_WS:
            if(wws_length(w) == 1){
                name = _ws_get_name(wws_head(w));
            } else {
                name = NULL;
                warn(
                    "Cannot get name from V_WS of length > 1 (%s:%d)",
                    __func__, __LINE__
                );
            }
            break;
    }
    return name;
}

bool w_string_equal(W* a, W* b){
    char* a_str = _ws_get_name(a);
    char* b_str = _ws_get_name(b);
    bool is_equal = false;
    if(a_str && b_str)
        is_equal = strcmp(a_str, b_str) == 0;
    return is_equal;
}

Ws* ws_recurse_path(W* w, W* p){
    Ws* rs = NULL;
    switch(w->cls){
        case C_NEST:
        case C_DEREF:
        case C_COMPOSON:
            rs = g_ws(w);
            break;
        case T_SECTION:
            rs = ws_new(w_new(P_WS, g_ws(g_rhs(w))));
            break;
        case T_PATH:
        case T_H0:
        case T_H1:
        case T_H2:
        case T_H3:
        case T_H4:
        case T_H5:
        case T_H6:
        case T_H7:
        case T_H8:
        case T_H9:
        case T_CHECK:
        case T_FAIL:
            rs =
                (wws_length(g_lhs(p)) == 1 || w_equal_lhs(p, w))
                    ? g_ws(g_rhs(w))
                    : NULL;
            break;
        default:
            break;
    }
    return rs;
}
