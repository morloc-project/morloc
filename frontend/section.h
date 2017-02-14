#ifndef __SECTION_H__
#define __SECTION_H__

#include <string.h>
#include <stdlib.h>

typedef struct Section{
    char* name;
    char* lang; 
} Section;

Section* section_new(char* name, char* lang);

Section* section_copy(Section* section);

#endif
