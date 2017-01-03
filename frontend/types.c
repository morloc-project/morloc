#include "types.h"

Manifold* manifold_new(){
    static int uid = 0;
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    m->uid = uid++;
    return m;
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

Couplet* couplet_new(struct W* lhs, struct W* rhs){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->lhs = lhs;
    c->rhs = rhs;
    return c;
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
