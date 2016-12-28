#include "selection.h"

Selection* selection_new(){
    Selection* s = (Selection*)calloc(1, sizeof(Selection));
    return s;
}

Selection* selection_put(Selection* selection, Path* path){
    if(!selection){
        selection = selection_new();
    } else if(selection->path){
        Selection* newsel = selection_new();
        newsel->path = selection->path;
        newsel->next = selection->next;
        selection->next = newsel;
    }
    selection->path = path;
    return selection;
}

Selection* selection_from_str(char* selection_str){
    char* s = selection_str;
    Selection* sel = selection_new();
    for(int i = 0; ; i++){
        if(s[i] == '\0'){
            sel = selection_put(sel, path_from_str(s));
            break;
        }
        else if(s[i] == ','){
            s[i] = '\0';
            sel = selection_put(sel, path_from_str(s));
            s = s + i + 1;
            i = 0;
        }
    }
    return sel;
}
