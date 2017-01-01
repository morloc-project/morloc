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

void ws_assert_class(const W* w, Class cls){
    if(w->cls != cls){
        fprintf(
            stderr,
            "Class assertion failed! Got %s, expected %s.\n",
            w_class_str(w->cls), w_class_str(cls)
        );
    }
}

void ws_assert_type(const W* w, VType type){
    VType t = get_value_type(w->cls);
    if(t != type){
        fprintf(
            stderr,
            "Type assertion failed! Got %s, expected %s.\n",
            w_type_str(t), w_type_str(type)
        );
    }
}

void ws_print_r(const Ws* ws, Ws*(*recurse)(const W*), int depth){

    if(!ws || !ws->head) return;

    for(W* w = ws->head; w; w = w->next){
        for(int i = 0; i < depth; i++){ printf("  "); }

        printf("%s\n", w_str(w));
        Ws* rs = recurse(w);

        if(!rs) continue;

        for(W* r = rs->head; r; r = r->next){
            ws_print_r(r->value.ws, recurse, depth+1);
        }
    }
}

void ws_print(const Ws* ws, Ws*(*recurse)(const W*)){
    ws_print_r(ws, recurse, 0);
}

int ws_length(const Ws* ws){
    if(!ws) return 0;
    int size = 0;
    for(W* w = ws->head; w; w = w->next){ size++; }
    return size;
}


Ws* ws_flatten( const Ws* ws, Ws*(*recurse)(const W*) ){
    return ws_rfilter(ws, recurse, w_keep_all);
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
            Ws* down = ws_rfilter(r->value.ws, recurse, criterion);
            result = ws_join(result, down);
        }
    }
    return result;
}

Ws* ws_prfilter(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W*, const W*),
    bool(*criterion)(const W*, const W*),
    W*(*nextval)(const W*)
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
            Ws* down = ws_prfilter(r->value.ws, nextval(p), recurse, criterion, nextval);
            result = ws_join(result, down);
        }
    }
    return result;
}

void ws_prmod(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W*, const W*),
    void(*mod)(const W*),
    W*(*nextval)(const W*)
){
    if(!ws || !ws->head) return;
    for(W* w = ws->head; w; w = w->next){
        mod(w);
        Ws* rs = recurse(w, p); 
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_prmod(r->value.ws, nextval(p), recurse, mod, nextval);
        }
    }
}

void ws_map_pmod(Ws* xs, Ws* ps, void(*pmod)(Ws*, W*)){
    for(W* p = ps->head; p; p = p->next){
        pmod(xs, p);
    }
}

Ws* ws_split_couplet(const W* c){
    ws_assert_type(c, V_COUPLET);
    Ws* result = NULL;
    W* paths = c->value.couplet->lhs;
    switch(paths->cls){
        case K_LIST:
            {
                for(W* p = paths->value.ws->head; p; p = p->next){
                    W* nc = w_isolate(c);
                    nc->value.couplet->lhs = p;
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

/*

link . (trace_paths . *B top) *B
B :: get_paths . get_couplets . top


Ws* ws_split_couplet(const W*);


void ws_prmod(
    const Ws* ws,
    const W* p,
    Ws*(*recurse)(const W*, const W*),
    void(*mod)(const W*),
    W*(*nextval)(const W*)
);

void ws_map_pmod(Ws* xs, Ws* ps, void(*pmod)(Ws*, W*));




Ws* get_couplets(Ws* top);
Ws* get_paths(Ws* UUU, W* couplet);
Ws* trace_paths(Ws* top, W* UUU, W* path)
void link(Ws* UUU, W* couplet, W* UUU, W* target);

ws_2cone(ws, get_couplets, get_paths, trace_paths, link);

top  - everything
xfilter - everything -> x:couplets
yfilter - x -> y:paths
zfilter - everything -> y -> z:targets
map - x -> z -> void


ws_filter_map(ws, RESOLVE_PATHS, COUPLE);

for each path in list
    filter out elements that match path
    map f over them

*/

// === nextval functions ============================================

W* w_next(W* w){ return w->next; }

W* w_identity(W* w){ return w; }

// === filter criteria ==============================================
// ------------------------------------------------------------------

bool w_is_manifold(const W* w){
    return w->cls == C_MANIFOLD;
}

bool w_keep_all(const W* w){
    return true;
}

bool w_name_match(const W* w, const W* p){
    return false; // STUB
}

// === recursion rules ==============================================
// ------------------------------------------------------------------

Ws* ws_recurse_most(const W* w){
    if(!w) return NULL;
    Ws* rs = NULL;
    switch(get_value_type(w->cls)){
        case V_WS:
            rs = ws_add_val(rs, P_WS, w->value.ws);
            break;
        case V_COUPLET:
            {
                W* lhs = w->value.couplet->lhs;
                if(get_value_type(lhs->cls) == V_WS){
                    rs = ws_add_val(rs, P_WS, lhs->value.ws);
                }
                W* rhs = w->value.couplet->rhs;
                if(get_value_type(rhs->cls) == V_WS){
                    rs = ws_add_val(rs, P_WS, rhs->value.ws);
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
            rs = ws_add_val(rs, P_WS, w->value.ws);
            break;
        default:
            break;
    }
    return rs;
}

Ws* ws_recurse_none(const W* w){
    return NULL;
}

// ------------------------------------------------------------------
// ==================================================================





// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

/* bool is_recursive(TType type);                              */
/*                                                             */
/* #define STCMP(e, i, t)           \                          */
/*     (                            \                          */
/*         (e)->id && (i) && (t) && \                          */
/*         id_cmp((e)->id, (i)) &&  \                          */
/*         (e)->type == (t)         \                          */
/*     )                                                       */
/*                                                             */
/* #define TCMP(e, t) ((e) && (e)->type == (t))                */
/*                                                             */
/* #define SCMP(e, i) ((e)->id && (i) && id_cmp((e)->id, (i))) */

// Table* table_clone(const Table* table){
//     if(!table) return NULL;
// 
//     Table* clone = NULL;
// 
//     for(Entry* e = table->head; e; e = e->next){
//         Id* id = id_clone(e->id);        
//         TType type = e->type;
//         Entry* eclone = entry_new(id, type, NULL);
// 
//         switch(type){
//             case T_PATH:
//             case C_COMPOSON:
//             case C_NEST:
//             case C_DEREF:
//                 eclone->value.table = table_clone(e->value.table);
//                 break;
//             case C_MANIFOLD:
//                 eclone->value.manifold = manifold_new();
//                 break;
//             case C_POSITIONAL:
//             case C_GRPREF:
//                 eclone->value.string = strdup(e->value.string);
//                 break;
//             default:
//                 fprintf(stderr, "ERROR: can't clone this; will only clone paths\n");
//                 exit(EXIT_FAILURE);
//                 break;
//         }
// 
//         clone = table_add(clone, eclone);
// 
//     }
//     return clone;
// }
// 
// 
// bool _table_dump_r(const Table* table, int depth){
// 
//     if(!table) return false;
// 
//     for(Entry* e = table->head; e; e = e->next){
//         for(int i = 0; i < depth; i++){
//             if(i%2 == 0){
//                 printf("  ");
//             } else {
//                 printf(". ");
//             }
//         }
//         entry_print(e);
//         if(is_recursive(e->type)){
//             _table_dump_r(e->value.table, depth + 1);
//         }
//     }
// 
//     return true;
// }
// 
// void table_dump(const Table* table){
//     printf(" ------------------------------------------- \n");
//     _table_dump_r(table, 0);
//     printf(" ------------------------------------------- \n");
// }
// 
// Table* _table_composon_io(const Entry* entry, bool is_input){
//     if(!entry){
//         return NULL;
//     }
//     if(!(TCMP(entry, C_COMPOSON) || TCMP(entry, C_NEST))){
//         fprintf(stderr,
//             "ERROR: input must be a composon in "
//             "table_composon_{in/out}puts, returning NULL.\n"); 
//         return NULL;
//     }
//     Table* result = NULL;
//     for(Entry* e = entry->value.table->head; e; e = e->next){
//         switch(e->type){
//             case C_MANIFOLD:
//             case C_POSITIONAL:
//             case C_DEREF:
//                 result = table_add(result, e);
//                 break;
//             case T_PATH:
//             case C_NEST:
//                 {
//                     // output comes from the first (outermost) composon of the
//                     // nested expression, while input goes to the last
//                     // (innermost).
//                     Entry* ne = is_input ? e->value.table->tail : e->value.table->head;
//                     result = table_join(result, _table_composon_io(ne, is_input));
//                 }
//                 break;
//             case C_GRPREF:
//                 fprintf(stderr, "Unresolved group reference at %s::%d\n", __func__, __LINE__);
//                 break;
//             default:
//                 fprintf(stderr, "Illegal type in composition\n");
//         }
//     }
//     return result;
// }
// 
// Table* table_composon_outputs(const Entry* entry){
//     return _table_composon_io(entry, false);
// }
// 
// Table* table_composon_inputs(const Entry* entry){
//     return _table_composon_io(entry, true);
// }
// 
// bool is_recursive(TType type){
//     return type == T_PATH ||
//            type == C_COMPOSON ||
//            type == C_NEST;
// }
// 
// Table* table_get(const Table* table, Id* id, TType type){
//     Table* out = NULL;
//     for(Entry* e = table->head; e; e = e->next){
//         if(STCMP(e, id, type)){
//             out = table_add(out, e);
//         }
//     }
//     return out;
// }
// 
// Table* table_recursive_get(const Table* table, Id* id, TType type){
//     Table* out = NULL;
//     for(Entry* e = table->head; e; e = e->next){
//         if(STCMP(e, id, type)){
//             out = table_add(out, e);
//         }
//         if(is_recursive(e->type)){
//             out = table_join(out, table_recursive_get(e->value.table, id, type)); 
//         }
//     }
//     return out;
// }
// 
// Table* table_path_get(const Table* table, Path* path, TType type){
//     Table* out = NULL;
// 
//     if(!table || !table->head) return NULL;
// 
//     for(Entry* e = table->head; e; e = e->next){
//         if(path_is_base(path)){
//             if(STCMP(e, path->id, type)){
//                 out = table_add(out, e);
//             }
//             if(is_recursive(e->type)){
//                 out = table_join(out, table_recursive_get(e->value.table, path->id, type));
//             }
//         } else {
//             if(                            // Recurse down if
//               is_recursive(e->type) &&     // 1. this entry is holds a table
//               (                            // 2. the Entry's table is either
//                 !(e->id && e->id->name) || //   a) anonymous (nest or composon)
//                 SCMP(e, path->id)          //   b) name matches path name
//               )
//             ){
//                 out = table_join(out, table_path_get(e->value.table, path->next, type));
//             }
//         }
//     }
//     return out;
// }
// 
// Table* table_selection_get(const Table* table, Selection* selection, TType type){
//     Table* out = NULL;
//     for(Selection* s = selection; s; s = s->next){
//         Table* b = table_path_get(table, s->path, type);
//         out = table_join(out, b);
//     }
//     return out;
// }
// 
// Table* table_recursive_get_type(const Table* table, TType type){
//     Table* out = NULL;
//     for(Entry* e = table->head; e; e = e->next){
//         if(TCMP(e, type)){
//             out = table_add(out, e);
//         }
//         if(is_recursive(e->type)){
//             Table* down = table_recursive_get_type(e->value.table, type);
//             out = table_join(out, down);
//         }
//     }
//     return out;
// }
// 
// #undef STCMP
// #undef TCMP
// #undef SCMP
