#include "ws.h"

// ------ local functions -------------------------------------------
void _join(Ws* a, Ws* b);
Ws* _ws_new(W* w);
void _retail(Ws* ws, W* w);
Ws* _ws_add(Ws* ws, W* w);
void _ws_print_r(Ws* ws, Ws*(*recurse)(W*), int depth);
// ------------------------------------------------------------------


Ws* ws_new(W* w){
    Ws* ws = (Ws*)calloc(1, sizeof(Ws));
    W* w2 = w_isolate(w);
    ws->head = w2;
    ws->last = w2;
    return ws;
}


void w_clone_value(W* w){
    if(!w) return;
    switch(get_value_type(w->cls)){
        case V_NONE:
            s_none(w);
            break;
        case V_STRING:
            s_string(w, strdup(g_string(w)));
            break;
        case V_WS:
            s_ws(w, ws_clone(g_ws(w)));
            break;
        case V_COUPLET:
            {
            Couplet* c = couplet_new(w_clone(g_lhs(w)), w_clone(g_rhs(w)), g_couplet(w)->op);
            w_clone_value(c->lhs);
            w_clone_value(c->rhs);
            s_couplet(w, c);
            }
            break;
        case V_LABEL:
            s_label(w, label_copy(g_label(w)));
            break;
        case V_MANIFOLD:
            {
            Manifold* m = manifold_clone(g_manifold(w));
            m->function = m->function ? strdup(m->function) : NULL;
            m->effect = ws_clone( m->effect );
            m->hook   = ws_clone( m->hook   );
            m->cache  = ws_clone( m->cache  );
            m->check  = ws_clone( m->check  );
            m->open   = ws_clone( m->open   );
            m->pack   = ws_clone( m->pack   );
            m->pass   = ws_clone( m->pass   );
            m->fail   = ws_clone( m->fail   );
            m->doc    = ws_clone( m->doc    );
            m->inputs = ws_clone( m->inputs );
            s_manifold(w, m);
            }
            break;
    }
}
Ws* ws_clone(Ws* ws){
    Ws* clone = NULL;
    if(!ws) return NULL;
    for(W* w = w_clone(ws->head); w; w = w->next){
        w_clone_value(w);
        clone = ws_add(clone, w);
    }
    return clone;
}

Ws* ws_copy(Ws* ws){
    Ws* copy = NULL;
    if(!ws) return copy;
    for(W* w = ws->head; w; w = w->next){
        copy = ws_add(copy, w_isolate(w));
    }
    return copy;
}

Ws* ws_add(Ws* ws, W* w){
    W* w2 = w_isolate(w);
    if(!ws){
        ws = _ws_new(w2);
    } else {
        if(ws->last){
            _retail(ws, w2);
        } else {
            warn("WARNING: cannot add to tailless table\n");
        }
    }
    return ws;
}

Ws* ws_add_val(Ws* ws, Class cls, void* v){
    W* w = w_new(cls, v);
    ws = _ws_add(ws, w);
    return ws;
}

Ws* ws_join(Ws* a, Ws* b){
    if(b && b->head){
        if(a && a->head){
            _join(a, ws_copy(b));
        } else {
            a = ws_copy(b);
        }
    }
    return a;
}

Ws* ws_tail(Ws* ws){
    if(ws_length(ws) < 2) return NULL;
    Ws* n = _ws_new(ws->head->next);
    n->last = ws->last;
    return n;
}

Ws* ws_init(Ws* ws){
    if(ws_length(ws) < 2) return NULL;
    Ws* n = _ws_new(w_clone(ws->head));
    for(W* last = n->head; last->next; last = last->next){
        n->last = last;
    }
    n->last->next = NULL;
    return n;
}

W* ws_head(Ws* ws){
    return ws->head;
}

/* Get last element of a table */
W* ws_last(Ws* ws){
    return ws->last;
}

int ws_length(Ws* ws){
    if(!ws || !ws->head) return 0;
    int size = 0;
    for(W* w = ws->head; w; w = w->next){ size++; }
    return size;
}

void ws_print(Ws* ws, Ws*(*recurse)(W*)){
    if(!ws){
        fprintf(stderr, "(empty list)\n");
    } else {
        _ws_print_r(ws, recurse, 0);
    }
}

char* w_str(W* w){
    if(!w) return NULL;
    char* s = (char*)malloc(1024 * sizeof(char));
    char* c = w_class_str(w->cls);
    switch(get_value_type(w->cls)){
        case V_NONE:
            sprintf(s, "%s", c);
            break;
        case V_STRING:
            sprintf(s, "%s(%s)", c, g_string(w));
            break;
        case V_WS:
            {
                int n = 0;
                for(W* a = g_ws(w)->head; a; a = a->next) { n++; }
                sprintf(s, "%s<n=%d>", c, n);
            }
            break;
        case V_COUPLET:
            sprintf(
                s, "%s :: %s | %s", c,
                w_str(g_lhs(w)),
                w_str(g_rhs(w))
            );
            break;
        case V_LABEL:
            sprintf(
                s, "%s(%s:%s)", c,
                g_label(w)->name,
                g_label(w)->label
            );
            break;
        case V_MANIFOLD:
            sprintf(s, "%s", c);
            break;
        default:
            warn("illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

// ====== PRIVATE FUNCTIONS ==========================================

void _join(Ws* a, Ws* b){
    a->last->next = b->head;
    a->last = b->last;
}

/* no copy constructor, no reset */
Ws* _ws_new(W* w){
    Ws* ws = (Ws*)calloc(1, sizeof(Ws));
    ws->head = w;
    ws->last = w;
    return ws;
}

/* checkless attachment */
void _retail(Ws* ws, W* w){
    ws->last->next = w;
    ws->last = w;
}

Ws* _ws_add(Ws* ws, W* w){
    if(!ws){
        ws = _ws_new(w);
    } else {
        if(ws->last){
            _retail(ws, w);
        } else {
            warn("WARNING: cannot add to tailless table\n");
        }
    }
    return ws;
}

void _ws_print_r(Ws* ws, Ws*(*recurse)(W*), int depth){
    if(!ws || !ws->head) return;
    for(W* w = ws->head; w; w = w->next){
        for(int i = 0; i < depth; i++){ fprintf(stderr, "  "); }
        fprintf(stderr, "%s\n", w_str(w));
        Ws* rs = recurse(w);
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            _ws_print_r(g_ws(r), recurse, depth+1);
        }
    }
}


W* wws_new(W* w){
    return w_new(P_WS, ws_new(w));
}
W* wws_clone(W* wws){
    if(!wws) return NULL;
    s_ws(wws, ws_clone(g_ws(wws)));
    return wws;
}
W* wws_add(W* wws, W* w){
    if(!wws) return wws;
    s_ws(wws, ws_add(g_ws(wws), w));
    return wws;
}
W* wws_add_val(W* wws, Class cls, void* v){
    Ws* ws = wws ? g_ws(wws): NULL;
    ws = ws_add_val(ws, cls, v); 
    if(!wws){
        wws = w_new(P_WS, ws);
    } else {
        s_ws(wws, ws);
    }
    return wws;
}
W* wws_join(W* a, W* b){
    s_ws(a, ws_join(g_ws(a), g_ws(b)));
    return a;
}
W* wws_tail(W* w){
   return w_new(P_WS, ws_tail(g_ws(w))); 
}
W* wws_init(W* w){
    return w_new(P_WS, ws_init(g_ws(w)));
}
W* wws_head(W* w){
    return ws_head(g_ws(w));
}
W* wws_last(W* w){
    return ws_last(g_ws(w));
}
int wws_length(W* w){
    return ws_length(g_ws(w));
}
void wws_print(W* w, Ws*(*recurse)(W*)){
    ws_print(g_ws(w), recurse);
}
