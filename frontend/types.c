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
