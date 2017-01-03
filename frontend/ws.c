#include "ws.h"

/* no copy constructor, no reset */
Ws* _ws_new(W* w){
    Ws* ws = (Ws*)calloc(1, sizeof(Ws));
    ws->head = w;
    ws->tail = w;
    return ws;
}

/* checkless attachment */
void _retail(Ws* ws, W* w){
    ws->tail->next = w;
    ws->tail = w;
}

Ws* _ws_add(Ws* ws, W* w){
    if(!ws){
        ws = _ws_new(w);
    } else {
        if(ws->tail){
            _retail(ws, w);
        } else {
            fprintf(stderr, "WARNING: cannot add to tailless table\n");
        }
    }
    return ws;
}

Ws* ws_new(const W* w){
    Ws* ws = (Ws*)calloc(1, sizeof(Ws));
    W* w2 = w_isolate(w);
    ws->head = w2;
    ws->tail = w2;
    return ws;
}

Ws* ws_add(Ws* ws, const W* w){
    W* w2 = w_isolate(w);
    if(!ws){
        ws = _ws_new(w2);
    } else {
        if(ws->tail){
            _retail(ws, w2);
        } else {
            fprintf(stderr, "WARNING: cannot add to tailless table\n");
        }
    }
    return ws;
}

Ws* ws_add_val(Ws* ws, Class cls, void* v){
    W* w = w_new(cls, v);
    ws = _ws_add(ws, w);
    return ws;
}

void _join(Ws* a, Ws* b){
    a->tail->next = b->head;
    a->tail = b->tail;
}

Ws* ws_join(Ws* a, Ws* b){
    if(b && b->head){
        if(a && a->head){
            _join(a, b);
        } else {
            a = b;
        }
    }
    return a;
}

Ws* ws_increment(const Ws* ws){
    Ws* n = NULL;
    if(ws && ws->head && ws->head->next){
        n = (Ws*)malloc(sizeof(Ws));
        n->head = ws->head->next;
        n->tail = ws->tail;
    }
    return n;
}

int ws_length(const Ws* ws){
    if(!ws || !ws->head) return 0;
    int size = 0;
    for(W* w = ws->head; w; w = w->next){ size++; }
    return size;
}

void ws_print_r(const Ws* ws, Ws*(*recurse)(const W*), int depth){

    if(!ws || !ws->head) return;

    for(W* w = ws->head; w; w = w->next){
        for(int i = 0; i < depth; i++){ printf("  "); }

        printf("%s\n", w_str(w));
        Ws* rs = recurse(w);

        if(!rs) continue;

        for(W* r = rs->head; r; r = r->next){
            ws_print_r(g_ws(r), recurse, depth+1);
        }
    }
}

void ws_print(const Ws* ws, Ws*(*recurse)(const W*)){
    ws_print_r(ws, recurse, 0);
}

char* w_str(const W* w){
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
                sprintf(s, "%s<n=%d>\n", c, n);
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
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}
