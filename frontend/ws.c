#include "ws.h"

/* no copy constructor, no reset */
Ws* _ws_new(W* w){
    Ws* ws = (Ws*)malloc(sizeof(Ws));
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
    Ws* ws = (Ws*)malloc(sizeof(Ws));
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

void ws_print_r(const Ws* ws, Ws*(*recurse)(W*), int depth){
    for(W* w = ws->head; w; w = w->next){
        for(int i = 0; i < depth; i++){
           printf("  "); 
        }
        printf("%s\n", w_str(w));
        Ws* rs = recurse(w);
        if(!rs) continue;
        for(W* r = rs->head; r; r = r->next){
            ws_print_r(r->value.ws, recurse, depth+1);
        }
    }
}

void ws_print(const Ws* ws, Ws*(*recurse)(W*)){
    ws_print_r(ws, recurse, 0);
}


// === recursion rules ==============================================
// ------------------------------------------------------------------

Ws* ws_recurse_ws(W* w){
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

Ws* ws_recurse_none(W* w){
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
// void _join(Table* a, Table* b){
//     a->tail->next = b->head;
//     a->tail = b->tail;
// }
// 
// Table* table_join(Table* a, Table* b){
//     if(b && b->head){
//         if(a && a->head){
//             _join(a, b);
//         } else {
//             a = b;
//         }
//     }
//     return a;
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
