#include "manifold.h"

int _manifold_uid(){
    static int uid = 0;
    return uid++;
}

Manifold* manifold_new(){
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    m->uid = _manifold_uid();
    return m;
}

Manifold* manifold_clone(Manifold* m){
   Manifold* new_m = (Manifold*)malloc(sizeof(Manifold));
   memcpy(new_m, m, sizeof(Manifold));
   new_m->uid = _manifold_uid();
   return new_m;
}
