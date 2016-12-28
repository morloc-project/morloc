#include "path.h"

Path* path_new(){
    Path* p = (Path*)calloc(1, sizeof(Path));
    return p;
}

/* Each new element is appended to the end. Since I don't store the last
 * element, I have to look it up in linear time. However, there will usually be
 * few elements in the path, so the simplicity of this approach wins. */
Path* path_put(Path* path, Id* id){
    if(path->id){
        Path* new = path_new();    
        new->id = id;
        Path* c = path;
        for( ; c->next; c = c->next) {}
        c->next = new;
    } else {
        path->id = id;
    }
    return path;
}

bool path_is_base(Path* path){
    return path->next == NULL;
}

Path* path_from_str(char* path_str){
    char* s = path_str;
    Path* p = path_new();
    for(int i = 0; ; i++){
        if(s[i] == '\0'){
            p = path_put(p, id_from_str(s));
            break;
        }
        else if(s[i] == '/'){
            s[i] = '\0';
            p = path_put(p, id_from_str(s));
            s = s + i + 1;
            i = 0;
        }
    }
    return p;
}
