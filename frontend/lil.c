#include "lil.h"

void print_manifold_lil(Manifold* m){
    if(m){
        printf("EMIT m%d\n", m->uid);
        if(m->function)
            printf("FUNC m%d %s\n", m->uid, m->function);
        if(m->inputs){
            for(Entry* i = m->inputs->head; i; i = i->next){
                printf("INPT m%d m%d\n", m->uid, i->value.manifold->uid);
            }
        }
        if(m->effect)
            printf("EFCT m%d %s\n", m->uid, m->effect);
    }
}

void print_prolog(Table* t_top){ }

void print_manifolds(Table* t_top){
    Table* t_man = table_recursive_get_type(t_top, C_MANIFOLD);
    for(Entry* e = t_man->head; e; e = e->next){
        print_manifold_lil(e->value.manifold);
        if(e->next){
            printf("\n");
        }
    }
}

void print_epilog(Table* t_top){ }

void print_lil(Table* t_top){
    if(t_top && t_top->head){
        print_prolog(t_top);
        print_manifolds(t_top);
        print_epilog(t_top);
    } else {
        fprintf(stderr, "The symbol table is empty - nothing to do\n");
    }
}
