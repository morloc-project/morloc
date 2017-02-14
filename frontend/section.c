#include "section.h"

Section* section_new(char* name, char* lang){
    Section* s = (Section*)malloc(1 * sizeof(Section));
    s->name = name;
    s->lang = lang;
    return s;
}

Section* section_copy(Section* section){
    Section* s = (Section*)malloc(1 * sizeof(Section));
    s->name = strdup(section->name);
    s->lang = strdup(section->lang);
    return s;
}
