#include "label.h"

Label* label_new(char* name){
    Label* p = (Label*)malloc(sizeof(Label));
    p->next = NULL;
    p->name = name; 
}
