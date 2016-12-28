#include "effect.h"

Effect* effect_new(Selection* selection, char* function){
    Effect* e = (Effect*)malloc(sizeof(Effect));
    e->selection = selection;
    e->function = function;
    return e;
}
