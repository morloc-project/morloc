#include "types.h"

NamedList* new_NamedList(){
    NamedList* l = (NamedList*)calloc(1, sizeof(NamedList));
    return l;
}

List* new_List(){
    List* l = (List*)calloc(1, sizeof(List));
    return l; 
}

Composon* new_Composon(ComposonType type){
    Composon* c = (Composon*)malloc(sizeof(Composon));
    c->type = type;
    return c;
}

Manifold* new_Manifold(char* name){
    static int uid = 0;
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    m->name = name;
    m->uid = uid++;
    return m;
}

void rewind_path(NamedList* p){
    for(NamedList* l = p; l; l = l->next){
        List* lc = (List*)l->value;
        for(List* ll = lc; ll; ll = ll->next){
            List* llc = (List*)ll->value;
            REWIND(llc);
        }
        REWIND(lc);
    }
    REWIND(p);
}
