#include "types.h"

Couplet* couplet_new(struct W* lhs, struct W* rhs){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->lhs = lhs;
    c->rhs = rhs;
    return c;
}

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

Label* label_new(){
    return (Label*)calloc(1, sizeof(Label));
}

Label* label_new_set(char* name, char* label){
    Label* l = (Label*)malloc(sizeof(Label));
    l->name = name;
    l->label = label;
    return l;
}

Label* label_copy(Label* l){
    char* name  = l->name  ? strdup(l->name)  : NULL;
    char* label = l->label ? strdup(l->label) : NULL;
    return label_new_set(name, label);
}

bool label_cmp(Label* a, Label* b){
    return
        a && b &&
        a->name && b->name &&
        strcmp(a->name, b->name) == 0 &&
        (
            (a->label == NULL && b->label == NULL) ||
            strcmp(a->label, b->label) == 0
        )
    ;
}
