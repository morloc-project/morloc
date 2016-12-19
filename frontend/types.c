#include "types.h"

Couplet* new_Couplet(char* name, char* value){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->name = name;
    c->value = value; 
    return c;
}

List* new_List(){
    List* l = (List*)calloc(1, sizeof(List));
    return l; 
}
