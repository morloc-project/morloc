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

var       [a-zA-Z]+(:[a-zA-Z0-9]+)?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*{ws}*::

identifier {var}{ws}*::

couple ::

ws        [ \t\n\r]
comment   #.*\n

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
    yylval.COMPOSON = id_from_str(yytext);
    LOG("COMPOSON");
    return COMPOSON;
}

<S_CPLT>{selection} {
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
        print_lil(table);
        fclose(toklog);
    }
    return status;
}
