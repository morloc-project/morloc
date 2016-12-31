#include "lil.h"

void print_manifold_lil(W* c_m){
    Manifold* m = c_m->value.couplet->rhs->value.manifold;

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
                    printf("INPM m%d %d m%d\n", m->uid, i++, w->value.manifold->uid);
                    break;
                case C_POSITIONAL:
                    printf("INPP m%d %d %s\n", m->uid, i++, w->value.string);
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

    if(m->effects){
        for(W* w = m->effects->head; w; w = w->next){
            printf("EFCT m%d %s\n", m->uid, w->value.string);
        }
    }
}

void print_prolog(Ws* ws_top){ }

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

void print_epilog(Ws* ws_top){ }

void print_lil(Ws* ws_top){
    if(ws_top && ws_top->head){
        print_prolog(ws_top);
        print_manifolds(ws_top);
        print_epilog(ws_top);
    } else {
        fprintf(stderr, "The symbol table is empty - nothing to do\n");
    }
}
