#include "hof.h"

Ws* ws_flatten( Ws* ws, Ws*(*recurse)(W*) ){
    return ws_rfilter(ws, recurse, w_keep_all);
}

Ws* ws_filter(Ws* ws, bool(*criterion)(W*)){
    return ws_rfilter(ws, ws_recurse_none, criterion);
}

Ws* ws_rfilter( Ws* ws, Ws*(*recurse)(W*), bool(*criterion)(W*) ){
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

Ws* ws_pfilter(Ws* ws, W* p, bool(*criterion)(W*, W*)){
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
    Ws* ws,
    W* p,
    Ws*(*recurse)(W* w, W* p),
    bool(*criterion)(W* w, W* p),
    W*(*nextval)(W* w, W* p)
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
    Ws* ws,
    W* p,
    Ws*(*recurse)(W* w, W* p),
    bool(*criterion)(W* w, W* p),
    void(*mod)(W* w, W* p),
    W*(*nextval)(W* w, W* p)
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
    Ws* ws,
    Ws*(*recurse)(W*),
    bool(*lc)(W*),
    bool(*rc)(W*),
    void(*mod)(W*, W*)
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
    Ws* ws,
    Ws* ps,
    Ws*(*recurse)(W*),
    bool(*criterion)(W*),
    void(*mod)(W* w, Ws* ps)
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

void ws_rcmod(
    Ws* ws,
    Ws*(*recurse)(W*),
    bool(*criterion)(W*),
    void(*mod)(W*)
){
    if(!ws || !ws->head) return;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w)){
            mod(w);
        }
        Ws* rs = recurse(w); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_rcmod(g_ws(r), recurse, criterion, mod);
        }
    }
}

W* ws_scrap(
    Ws* ws,
    W* st,
    Ws*(*recurse)(W*),
    bool(*criterion)(W*),
    W*(*mod)(W*, W*)
){
    if(!ws || !ws->head) return st;
    for(W* w = ws->head; w; w = w->next){
        if(criterion(w)){
            st = mod(w, st);
        }
        Ws* rs = recurse(w); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            st = ws_scrap(g_ws(r), st, recurse, criterion, mod);
        }
    }
    return st;
}

void ws_map_pmod(Ws* xs, Ws* ps, void(*pmod)(Ws*, W*)){
    if(!ps) return;
    for(W* p = ps->head; p; p = p->next){
        pmod(xs, p);
    }
}

Ws* ws_split_couplet(W* c){
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

Ws* ws_map_split(Ws* ws, Ws*(*split)(W*)){
    if(!ws) return NULL;
    Ws* result = NULL;
    for(W* w = ws->head; w; w = w->next){
       result = ws_join(result, split(w)); 
    }
    return result;
}

void ws_mod(Ws* ws, void(*mod)(W*)){
    if(!ws) return;
    for(W* w = ws->head; w; w = w->next){
       mod(w); 
    }
}

void ws_2mod(Ws* xs, Ws* ys, void(*mod)(W*, W*)){
    if(!(xs && ys)) return;
    for(W* x = xs->head; x; x = x->next){
        for(W* y = ys->head; y; y = y->next){ 
            mod(x, y);
        }
    }
}

void ws_3mod(Ws* xs, Ws* ys, Ws* zs, void(*mod)(W* x, W* y, W* z)){
    if(!(xs && ys && zs)) return;
    for(W* x = xs->head; x; x = x->next){
        for(W* y = ys->head; y; y = y->next){ 
            for(W* z = zs->head; z; z = z->next){ 
                mod(x, y, z);
            }
        }
    }
}

Ws* ws_map(Ws*  xs, W*(*fun)(W*)){
    Ws* ys = NULL;
    if(!xs) return ys;
    for(W* w = xs->head; w; w = w->next){
       ys = ws_add(xs, fun(w));
    }
    return ys;
}

Ws* ws_m2n_map(Ws* xs, Ws* ys, Ws*(*fun)(W* x, Ws* ys)){
    if(!xs) return NULL;

    for(W* x = xs->head; x; x = x->next){
        ys = fun(x, ys);
    }
    return ys;
}

void ws_zip_mod(Ws* xs, Ws* ys, void(*mod)(W*, W*)){
    if(!(xs && ys)) return;
    if(ws_length(xs) != ws_length(ys)){
        fprintf(stderr, "Cannot zipmod over lists of unequal lengths\n");
        return;
    }
    W* x = xs->head;
    W* y = ys->head;
    for(; x && y; x = x->next, y = y->next){
        mod(x, y);
    }
}

W* ws_szap(Ws* xs, Ws* ys, W* st, W*(*mod)(W*, W*, W*)){
    if(!(xs && ys)) return st;
    if(ws_length(xs) != ws_length(ys)){
        fprintf(stderr, "Cannot zipmod over lists of unequal lengths\n");
        return st;
    }
    W* x = xs->head;
    W* y = ys->head;
    for(; x && y; x = x->next, y = y->next){
        st = mod(x, y, st);
    }
    return st;
}


void ws_pmod(Ws* ws, W* p, void(*mod)(W*, W*)){
    for(W* w = ws->head; w; w = w->next){
       mod(w, p); 
    }
}

void ws_filter_mod(Ws* top,
    Ws*(*xfilter)(Ws*),
    void(*mod)(W* x)
){
    Ws* xs = xfilter(top);
    ws_mod(xs, mod);
}

void ws_filter_2mod(Ws* top,
    Ws*(*xfilter)(Ws*),
    Ws*(*yfilter)(Ws*),
    void(*mod)(W* x, W* y)
){
    Ws* xs = xfilter(top);
    Ws* ys = yfilter(top);
    ws_2mod(xs, ys, mod);
}

void ws_filter_3mod(Ws* top,
    Ws*(*xfilter)(Ws*),
    Ws*(*yfilter)(Ws*),
    Ws*(*zfilter)(Ws*),
    void(*mod)(W* x, W* y, W* z)
){
    Ws* xs = xfilter(top);
    Ws* ys = yfilter(top);
    Ws* zs = zfilter(top);
    ws_3mod(xs, ys, zs, mod);
}


void ws_cone(Ws* top,
    Ws*(*xfilter)(Ws*),
    Ws*(*yfilter)(Ws*, W*),
    void(*mod)(Ws*, W* x, W* y)
){
    Ws* xs = xfilter(top);
    for(W* x = xs->head; x; x = x->next){
        Ws* ys = yfilter(top, x);
        for(W* y = ys->head; y; y = y->next){ 
            mod(top, x, y);
        }
    }
}

void ws_2cone(Ws* top,
    Ws*(*xfilter)(Ws* top),
    Ws*(*yfilter)(Ws* top, W* x),
    Ws*(*zfilter)(Ws* top, W* x, W* y),
    void(*mod)(Ws* top, W* x, W* y, W* z)
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

W* w_nextval_always(W* w, W* p){ return p->next; }

W* w_nextval_never(W* w, W* p){ return p; }

/* p a modifier (e.g. effect).
 * w a node into which we are recursing
 *
 * if w is a path, we need to pop the top level of p's lhs.
 */
W* w_nextval_ifpath(W* w, W* p) {
    W* next = NULL;
    if(w->cls == T_PATH && ws_length(g_ws(g_lhs(p))) > 1){
        W* lhs = g_lhs(p);
        switch(lhs->cls){
            case K_PATH:
                next = w_isolate(p);
                s_ws(g_lhs(next), ws_tail(g_ws(lhs)));
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

bool w_is_manifold(W* w){
    return w ? w->cls == C_MANIFOLD : false;
}

bool w_is_type(W* w){
    return w ? w->cls == T_TYPE : false;
}

bool w_is_composon(W* w){
    return w ? w->cls == C_COMPOSON : false;
}

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

bool ws_cmp_lhs(W* a, W* b){

    Label* a_label = _ws_get_label_from_lhs(g_lhs(a));
    Label* b_label = _ws_get_label_from_lhs(g_lhs(b));

    return label_cmp(a_label, b_label);
}

Ws* ws_recurse_path(W* w, W* p){

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
