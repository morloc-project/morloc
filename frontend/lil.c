#include "lil.h"

void print_manifold_lil(W* c_m){
    Manifold* m = g_manifold(g_rhs(c_m));

    if(!m) return;

    printf("EMIT m%d\n", m->uid);

    if(m->function){
        printf("FUNC m%d %s\n", m->uid, m->function);
    }

    if(m->inputs){
        int i = 0;
        for(W* w = m->inputs->head; w; w = w->next){
            switch(w->cls){
                case C_MANIFOLD:
                    printf("INPM m%d %d m%d\n", m->uid, i++, g_manifold(g_rhs(w))->uid);
                    break;
                case C_POSITIONAL:
                    printf("INPP m%d %d %s\n", m->uid, i++, g_string(w));
                    break;
                case C_DEREF:
                    printf("NORM d%d\n", w->uid);
                    // STUB
                    // - build the whole graph
                    // - specify inputs
                    // - check for singular output
                    printf("INPD m%d %d d%d\n", m->uid, i++, w->uid);
                    break;
                default:
                    break;
            }
        }
    }

    if(m->effect){
        for(W* w = m->effect->head; w; w = w->next){
            printf("EFCT m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->cache){
        for(W* w = m->cache->head; w; w = w->next){
            printf("CACHE m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->check){
        for(W* w = m->check->head; w; w = w->next){
            printf("CHECK m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->open){
        for(W* w = m->open->head; w; w = w->next){
            printf("OPEN m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->pack){
        for(W* w = m->pack->head; w; w = w->next){
            printf("PACK m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->pass){
        for(W* w = m->pass->head; w; w = w->next){
            printf("PASS m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->fail){
        for(W* w = m->fail->head; w; w = w->next){
            printf("FAIL m%d %s\n", m->uid, g_string(w));
        }
    }

    if(m->doc){
        for(W* w = m->doc->head; w; w = w->next){
            printf("DOC m%d %s\n", m->uid, g_string(w));
        }
    }

}

void print_prolog(Ws* ws_top){
    for(W* e = ws_top->head; e; e = e->next){
        if(e->cls == T_SOURCE){
            // I add an empty string at the initialization of each
            // source object, so if there is no code, the length will be 1,
            // not 0.
            if(ws_length(g_ws(g_rhs(e))) > 1){
                printf("SOURCE %s BEGIN\n", g_string(g_lhs(e)));
                for(W* line = g_ws(g_rhs(e))->head->next; line; line = line->next){
                    printf("   %s\n", g_string(line));
                }
            }
        }
    }
}

void print_manifolds(Ws* ws_top){
    Ws* ws_man = ws_rfilter(ws_top, ws_recurse_most, w_is_manifold);
    if(!ws_man) return;
    for(W* w = ws_man->head; w; w = w->next){
        print_manifold_lil(w);
        if(w->next){
            printf("\n");
        }
    }
}

void print_epilog(Ws* ws_top){
    for(W* e = ws_top->head; e; e = e->next){
        if(e->cls == T_EXPORT){
            Label* l = g_label(e);
            if(l->name){
                char* alias = l->label ? l->label : l->name;
                printf("EXPORT %s %s\n", l->name, alias);
            }
        }
    }
}

void print_lil(Ws* ws_top){
    if(ws_top && ws_top->head){
        print_prolog(ws_top);
        print_manifolds(ws_top);
        print_epilog(ws_top);
    } else {
        fprintf(stderr, "The symbol table is empty - nothing to do\n");
    }
}
