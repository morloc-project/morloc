#include "util.h"

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
