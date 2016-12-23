#include "list.h"

NamedList* namedlist_new(){
    NamedList* l = (NamedList*)calloc(1, sizeof(NamedList));
    return l;
}

List* list_new(){
    List* l = (List*)calloc(1, sizeof(List));
    return l; 
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

void print_couplet(NamedList* l, char* cmd){
    if(l && l->value){
        for( ; l; l = l->next ){
            printf("%s %s %s\n", cmd, l->name->name, l->value);
        }
    }
}

void print_list(List* l, char* cmd){
    if(l && l->value){
        for( ; l; l = l->next ){
            printf("%s %s\n", cmd, l->value);
        }
    }
}
