#include "manifold.h"

LL* new_LL(){
    return (LL*)calloc(1, sizeof(LL));
}

Arg* new_Arg(){
    return (Arg*)calloc(1, sizeof(Arg));
}

Cache* new_Cache(){
    return (Cache*)calloc(1, sizeof(Cache));
}

Manifold* new_Manifold(void){
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    return m;
}


void free_LL(LL* ll, void (*free_value)(void*)){
    if(ll){
        // fast forward as needed
        while(ll->next)
            ll = ll->next;
        while(ll){
            LL* ll_prev = ll->prev; 
            free_value(ll->value); 
            free(ll);
            ll = ll_prev;
        }
    }
}

void vfree_char(void* vptr){
    char* value = (char*) vptr;
    if(value)
        free(value);
}

void vfree_Arg(void* vptr){
    Arg* arg = (Arg*) vptr;
    if(arg->lhs)
        free(arg->lhs);
    if(arg->rhs)
        free(arg->rhs);
    if(arg->rhs_type)
        free(arg->rhs_type);
    free(arg);
}

void vfree_Cache(void* vptr){
    Cache* cache = (Cache*) vptr;
    free_LL(cache->args, vfree_Arg);
    if(cache->name)
        free(cache->name);
    free(cache);
}

void vfree_Manifold(void* vptr){
    Manifold* m = (Manifold*) vptr;
    if(m){
        free_LL( m->name,     vfree_char     );
        free_LL( m->function, vfree_char     );
        free_LL( m->cache,    vfree_Cache    );
        free_LL( m->args,     vfree_Arg      );
        free_LL( m->inputs,   vfree_Manifold );
        free_LL( m->checks,   vfree_Manifold );
        free_LL( m->effects,  vfree_Manifold );
        free_LL( m->label,    vfree_char     );
        free_LL( m->type,     vfree_char     );
        free_LL( m->unpack,   vfree_char     );
        free_LL( m->run,      vfree_char     );
        free_LL( m->fail,     vfree_char     );
        free_LL( m->pack,     vfree_char     );
        free(m);
        m = NULL;
    }
}

LL* add(LL* ll, void* value){
    ll->next = new_LL();
    ll->next->prev = ll;
    ll->next->value = value;
    return ll->next;
}
