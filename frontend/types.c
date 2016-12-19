#include "types.h"

Couplet* new_Couplet(char* name, char* value){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->name = name;
    c->value = value; 
    return c;
}

Source* new_Source(char* lang){
    Source* s = (Source*)malloc(1 * sizeof(Source));
    s->lang = lang;
    s->lines = new_List();
}

List* new_List(){
    List* l = (List*)calloc(1, sizeof(List));
    return l; 
}
