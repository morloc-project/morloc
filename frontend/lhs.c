#include "lhs.h"

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

    return newstring;
}

W* label_from_str(char* s){
    Label* id = (Label*)calloc(1, sizeof(Label));
    int i = 0;
    for(;;i++){
        if(s[i] == '\0'){
            break;
        }
        else if(s[i] == ':'){
            id->label = strdup(s + i + 1);
            break;
        }
    }
    id->name = (char*)malloc((i+1) * sizeof(char));
    id->name = memcpy(id->name, s, i*sizeof(char));
    id->name[i] = '\0';

    id->name = trim_ws(id->name);
    id->label = trim_ws(id->label);

    W* w = w_new(K_LABEL, id);

    return w;
}

W* path_from_str(char* path_str){
    char* s = path_str;
    Ws* p = NULL;
    for(int i = 0; ; i++){
        if(s[i] == '\0'){
            p = ws_add(p, label_from_str(s));
            break;
        }
        else if(s[i] == '/'){
            s[i] = '\0';
            p = ws_add(p, label_from_str(s));
            s = s + i + 1;
            i = 0;
        }
    }

    W* w = w_new(K_PATH, p);

    return w;
}

W* list_from_str(char* list_str){
    char* s = list_str;
    Ws* ws = NULL;
    for(int i = 0; ; i++){
        if(s[i] == '\0'){
            ws = ws_add(ws, path_from_str(s));
            break;
        }
        else if(s[i] == ','){
            s[i] = '\0';
            ws = ws_add(ws, path_from_str(s));
            s = s + i + 1;
            i = 0;
        }
    }

    W* w = w_new(K_LIST, ws);

    return w;
}

/* bool path_is_base(Path* path){                                                     */
/*     return path->next == NULL;                                                     */
/* }                                                                                  */
/*                                                                                    */
/* Id* id_clone(Id* id){                                                              */
/*                                                                                    */
/*     if(!id) return NULL;                                                           */
/*                                                                                    */
/*     Id* newid = id_new();                                                          */
/*     newid->name = id->name ? strdup(id->name) : NULL;                              */
/*     newid->label = id->label ? strdup(id->label) : NULL;                           */
/*                                                                                    */
/*     return newid;                                                                  */
/* }                                                                                  */
/*                                                                                    */
/* bool id_cmp(Id* a, Id* b){                                                         */
/*     if(!a || !b){                                                                  */
/*         fprintf(stderr, "WARNING: cannot compare null ids\n"); fflush(stderr);     */
/*     }                                                                              */
/*     if(!a->name || !b->name){                                                      */
/*         fprintf(stderr, "WARNING: cannot compare nameless ids\n"); fflush(stderr); */
/*     }                                                                              */
/*     bool result =                                                                  */
/*         strcmp(a->name, b->name) == 0 &&                                           */
/*         (                                                                          */
/*             (a->label == NULL && b->label == NULL) ||                              */
/*             (                                                                      */
/*                 (a->label != NULL && b->label != NULL) &&                          */
/*                 strcmp(a->label, b->label) == 0                                    */
/*             )                                                                      */
/*         );                                                                         */
/*     return result;                                                                 */
/* }                                                                                  */
