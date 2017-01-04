#include "hof.h"

Ws* ws_flatten( const Ws* ws, Ws*(*recurse)(const W*) ){
    return ws_rfilter(ws, recurse, w_keep_all);
}

Ws* ws_filter(const Ws* ws, bool(*criterion)(const W*)){
    return ws_rfilter(ws, ws_recurse_none, criterion);
}

Ws* ws_rfilter( const Ws* ws, Ws*(*recurse)(const W*), bool(*criterion)(const W*) ){
    Ws* result = NULL;
    if(!ws || !ws->head) return NULL;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w)){
            result = ws_add(result, w);
        }
        Ws* rs = recurse(w);
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            Ws* down = ws_rfilter(g_ws(r), recurse, criterion);
            result = ws_join(result, down);
        }
    }
    return result;
}

Ws* ws_pfilter(const Ws* ws, const W* p, bool(*criterion)(const W*, const W*)){
    Ws* result = NULL;
    if(!ws || !ws->head) return NULL;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w, p)){
            result = ws_add(result, w);
        }
    }
    return result;
}

Ws* ws_prfilter(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W* w, const W* p),
    bool(*criterion)(const W* w, const W* p),
    const W*(*nextval)(const W* w, const W* p)
){
    Ws* result = NULL;
    if(!ws || !ws->head) return NULL;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w, p)){
            result = ws_add(result, w);
        }
        Ws* rs = recurse(w, p); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            Ws* down = ws_prfilter(g_ws(r), nextval(w, p), recurse, criterion, nextval);
            result = ws_join(result, down);
        }
    }
    return result;
}

void ws_prmod(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W* w, const W* p),
    bool(*criterion)(const W* w, const W* p),
    void(*mod)(const W* w, const W* p),
    const W*(*nextval)(const W* w, const W* p)
){
    if(!ws || !ws->head) return;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w,p)){
            mod(w, p);
        }
        Ws* rs = recurse(w, p); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_prmod(g_ws(r), nextval(w, p), recurse, criterion, mod, nextval);
        }
    }
}

void ws_recursive_reduce_mod(
    const Ws* ws,
    Ws*(*recurse)(const W*),
    bool(*lc)(const W*),
    bool(*rc)(const W*),
    void(*mod)(const W*, const W*)
){
    if(!ws || !ws->head) return;
    for(W* a = ws->head; a; a = a->next){
        W* b = a->next; 
        if(lc(a) && rc(b)){
            mod(a, b);
        }
        Ws* rs = recurse(a); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_recursive_reduce_mod(g_ws(r), recurse, lc, rc, mod);
        }
    }
}

void ws_ref_rmod(
    const Ws* ws,
    const Ws* ps,
    Ws*(*recurse)(const W*),
    bool(*criterion)(const W*),
    void(*mod)(W* w, const Ws* ps)
){
    if(!ws || !ws->head) return;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w)){
            mod(w, ps);
        }
        Ws* rs = recurse(w); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_ref_rmod(g_ws(r), ps, recurse, criterion, mod);
        }
    }
}

void ws_map_pmod(Ws* xs, const Ws* ps, void(*pmod)(Ws*, const W*)){
    if(!ps) return;
    for(W* p = ps->head; p; p = p->next){
        pmod(xs, p);
    }
}

Ws* ws_split_couplet(const W* c){
    Ws* result = NULL;
    W* paths = g_lhs(c);
    switch(paths->cls){
        case K_LIST:
            {
                for(W* p = g_ws(paths)->head; p; p = p->next){
                    W* nc = w_isolate(c);
                    s_lhs(nc, p);
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
            fprintf(stderr, "ERROR: invalid lhs type in couplet (%s:%d)", __func__, __LINE__);
            break;
    }
    return result;
}

Ws* ws_map_split(const Ws* ws, Ws*(*split)(const W*)){
    if(!ws) return NULL;
    Ws* result = NULL;
    for(W* w = ws->head; w; w = w->next){
       result = ws_join(result, split(w)); 
    }
    return result;
}

void ws_mod(const Ws* ws, void(*mod)(const W*)){
    if(!ws) return;
    for(W* w = ws->head; w; w = w->next){
       mod(w); 
    }
}

void ws_2mod(const Ws* xs, const Ws* ys, void(*mod)(const W*, const W*)){
    if(!(xs && ys)) return;
    for(W* x = xs->head; x; x = x->next){
        for(W* y = ys->head; y; y = y->next){ 
            mod(x, y);
        }
    }
}

void ws_3mod(const Ws* xs, const Ws* ys, const Ws* zs, void(*mod)(const W* x, const W* y, const W* z)){
    if(!(xs && ys && zs)) return;
    for(W* x = xs->head; x; x = x->next){
        for(W* y = ys->head; y; y = y->next){ 
            for(W* z = zs->head; z; z = z->next){ 
                mod(x, y, z);
            }
        }
    }
}

void ws_pmod(const Ws* ws, const W* p, void(*mod)(const W*, const W*)){
    for(W* w = ws->head; w; w = w->next){
       mod(w, p); 
    }
}

void ws_filter_mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    void(*mod)(const W* x)
){
    Ws* xs = xfilter(top);
    ws_mod(xs, mod);
}

void ws_filter_2mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    void(*mod)(const W* x, const W* y)
){
    Ws* xs = xfilter(top);
    Ws* ys = yfilter(top);
    ws_2mod(xs, ys, mod);
}

void ws_filter_3mod(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*),
    Ws*(*zfilter)(const Ws*),
    void(*mod)(const W* x, const W* y, const W* z)
){
    Ws* xs = xfilter(top);
    Ws* ys = yfilter(top);
    Ws* zs = zfilter(top);
    ws_3mod(xs, ys, zs, mod);
}


void ws_cone(const Ws* top,
    Ws*(*xfilter)(const Ws*),
    Ws*(*yfilter)(const Ws*, const W*),
    void(*mod)(const Ws*, const W* x, const W* y)
){
    Ws* xs = xfilter(top);
    for(W* x = xs->head; x; x = x->next){
        Ws* ys = yfilter(top, x);
        for(W* y = ys->head; y; y = y->next){ 
            mod(top, x, y);
        }
    }
}

void ws_2cone(const Ws* top,
    Ws*(*xfilter)(const Ws* top),
    Ws*(*yfilter)(const Ws* top, const W* x),
    Ws*(*zfilter)(const Ws* top, const W* x, const W* y),
    void(*mod)(const Ws* top, const W* x, const W* y, const W* z)
){
    Ws* xs = xfilter(top);
    for(W* x = xs->head; x; x = x->next){
        Ws* ys = yfilter(top, x);
        for(W* y = ys->head; y; y = y->next){ 
            Ws* zs = zfilter(top, x, y);
            for(W* z = zs->head; z; z = z->next){ 
                mod(top, x, y, z);
            }
        }
    }
}

// === nextval functions ============================================

const W* w_nextval_always(const W* w, const W* p){ return p->next; }

const W* w_nextval_never(const W* w, const W* p){ return p; }

/* p a modifier (e.g. effect).
 * w a node into which we are recursing
 *
 * if w is a path, we need to pop the top level of p's lhs.
 */
const W* w_nextval_ifpath(const W* w, const W* p) {
    W* next = NULL;
    if(w->cls == T_PATH && ws_length(g_ws(g_lhs(p))) > 1){
        W* lhs = g_lhs(p);
        switch(lhs->cls){
            case K_PATH:
                next = w_isolate(p);
                s_ws(g_lhs(next), ws_increment(g_ws(lhs)));
                break;
            case K_LIST:
                next = NULL;
                fprintf(stderr, "Not supported (%s:%d)", __func__, __LINE__);
                break;
            default:
                next = NULL;
                break;
        }
    } else {
        next = w_isolate(p);
    }
    return next;
}

// === filter criteria ==============================================
// ------------------------------------------------------------------

bool w_is_manifold(const W* w){
    return w ? w->cls == C_MANIFOLD : false;
}

bool w_is_composon(const W* w){
    return w ? w->cls == C_COMPOSON : false;
}

bool w_keep_all(const W* w){
    return true;
}

// === recursion rules ==============================================
// NOTE: recursion rules are splits
// ------------------------------------------------------------------

Ws* ws_recurse_most(const W* w){
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

Ws* ws_recurse_ws(const W* w){
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

Ws* ws_recurse_none(const W* w){
    return NULL;
}

Ws* ws_recurse_composition(const W* w){
    if(!w) return NULL;
    Ws* rs = NULL;
    switch(w->cls){
        case C_COMPOSON:
        case C_NEST:
            rs = ws_add_val(rs, C_NEST, g_ws(w));
            break;
        case T_PATH:
            rs = ws_add_val(rs, C_NEST, g_ws(g_rhs(w)));
            break;
        default:
            return NULL;
    }
    return rs;
}

Label* _ws_get_label_from_lhs(W* a){
    if(!a) return NULL;
    Label* label = NULL;
    switch(a->cls){
        case K_NAME:
            label = label_new_set(strdup(g_string(a)), NULL);
            break;
        case K_LABEL:
            label = g_label(a);
            break;
        case K_PATH:
            label = g_ws(a) ? g_label(g_ws(a)->head) : NULL;
            break;
        case K_LIST:
            label = NULL;
            fprintf(stderr, "Recursion into K_LIST not supported (%s:%d)", __func__, __LINE__);
            break;
        default:
            label = NULL;
            fprintf(stderr, "Illegal left hand side (%s:%d)", __func__, __LINE__);
            break;
    }
    return label;
}

bool ws_cmp_lhs(const W* a, const W* b){

    Label* a_label = _ws_get_label_from_lhs(g_lhs(a));
    Label* b_label = _ws_get_label_from_lhs(g_lhs(b));

    return label_cmp(a_label, b_label);
}

Ws* ws_recurse_path(const W* w, const W* p){

    w_assert_type(p, V_COUPLET);

    switch(w->cls){
        case C_NEST:
            return g_ws(w);
        case T_PATH:
            return
                ws_length(g_ws(g_lhs(p))) == 1 || ws_cmp_lhs(w, p) ?
                g_ws(g_rhs(w)) : NULL;
        default:
            return NULL;
    }
}
