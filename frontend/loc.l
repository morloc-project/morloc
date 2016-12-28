%{
#include <stdio.h>
#include <ctype.h>

#include "loc.tab.h"

int newfile(char *fn);
int popfile(void);

FILE* toklog;
#define LOG(x) fprintf(toklog, x); fflush(toklog);
#define LOGV(x,a) fprintf(toklog, x, a); fflush(toklog);
%}

%option header-file="lex.yy.h"
%option noyywrap

%s S_PATH
%s S_CPLT

ws        [ \t\n\r]
comment   #.*\n

name      [a-zA-Z_][a-zA-Z0-9_]*
var       {name}(:{name})?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*{ws}*::

identifier {var}{ws}*::

grpref \*{var}

couple ::

%%

^@path   { LOG("SECTION_PATH"); BEGIN(S_PATH); return SECTION_PATH;   }
^@effect { LOG("SECTION_EFFECT"); BEGIN(S_CPLT); return SECTION_EFFECT; }

{ws} { LOGV("%s", yytext); }

{comment} { }

{couple} { LOG("COUPLE"); return COUPLE; }

<S_PATH>{identifier} {
    char* s = strdup(yytext);
    int i = 0;
    for(;;i++){
        char c = s[i];
        if(isspace(c) || c == ':')
            break;
    }
    for(int j = strlen(s) - 1; j >= i ; j--){
        unput(s[j]);
    }
    s[i] = '\0';
    yylval.IDENTIFIER = id_from_str(s);
    LOG("IDENTIFIER");
    return IDENTIFIER;
}

<S_PATH>{var} {
    Id* id = id_from_str(yytext);
    Manifold* m = manifold_new();
    m->function = id->name;
    yylval.COMPOSON = entry_new(id, C_MANIFOLD, m);
    LOG("C_MANIFOLD");
    return COMPOSON;
}
<S_PATH>{grpref} {
    char* s = strdup(yytext + 1);
    Id* id = id_from_str(s);
    yylval.COMPOSON = entry_new(id, C_GRPREF, s);
    LOG("C_GRPREF");
    return COMPOSON;
}

<S_CPLT>{selection} {
    char* s = strdup(yytext);
    int N = strlen(s);
    int i = N - 1;
    for(;;i--){
        char c = s[i];
        if(!(isspace(c) || c == ':'))
            break;
    }
    for(int j = N - 1; j > i ; j--){
        unput(s[j]);
    }
    s[i+1] = '\0';

    LOG("SELECTION");
    yylval.SELECTION = selection_from_str(s);
    return SELECTION;
}

{var} { 
    LOG("VARIABLE");
    yylval.VARIABLE = strdup(yytext);
    return VARIABLE;
}

.    { LOGV("%c ", yytext[0]); return yytext[0]; }

%%

#include "bufstack.h"

int main(int argc, char ** argv){
    if(argc < 2){
        perror("Please provide a filename\n");
        return 1;
    }
    int status = 1;
    if(newfile(argv[1])){
        toklog = fopen("tok.log", "w");
        status = yyparse();
        build_manifolds(global_table);
        print_lil(global_table);
        fclose(toklog);
    }
    return status;
}
