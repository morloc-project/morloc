#include "manifold.h"

typedef struct Manifold{
    char* name;
    char* label;
    char* function;
    char** type;
    Manifold** inputs;
    Manifold** checks;
    Manifold** effects;
    char* cache;
    char* unpack;
    char* run;
    char* fail;
    char* pack;
} Manifold;

Manifold* new_manifold(void){
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    return m;
}

void free_manifold(Manifold* m){
    if(m) {
        if(m->name)     free(m->name);
        if(m->label)    free(m->label);
        if(m->function) free(m->function);
        if(m->type)     free(m->type);
        if(m->inputs)   free(m->inputs);
        if(m->checks)   free(m->checks);
        if(m->effects)  free(m->effects);
        if(m->cache)    free(m->cache);
        if(m->unpack)   free(m->unpack);
        if(m->run)      free(m->run);
        if(m->fail)     free(m->fail);
        if(m->pack)     free(m->pack);
        free(m);
        m = NULL;
    }
}
