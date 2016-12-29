char* trim_ws(char* s){
    if(!s) return NULL;

    int N = strlen(s);

    // Set string start to first non-whitespace character OR end of string
    int a = 0;
    while(isspace(s[a]) && a < N) a++;

    // Set string end to last non-whitespace character
    int b = strlen(s) - 1;
    while(isspace(s[b]) && b > a) b--;

    int n = b - a + 1;

    char* newstring = (char*)malloc((n+1)*sizeof(char));
    memcpy(newstring, s + a, n);
    newstring[n] = '\0';

    free(s);

    return newstring;
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

bool path_is_base(Path* path){
    return path->next == NULL;
}
