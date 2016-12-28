#ifndef __EFFECT_H__
#define __EFFECT_H__

#include <stdlib.h>

#include "selection.h"

typedef struct Effect{
    Selection* selection;
    char* function; 
} Effect;

Effect* effect_new(Selection* selection, char* function);

#endif
