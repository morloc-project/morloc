%{
#include <stdio.h>

#include "loc.tab.h"

int newfile(char *fn);
int popfile(void);

FILE* toklog;
#define LOG(x) fprintf(toklog, x); fflush(toklog);
#define LOGV(x,a) fprintf(toklog, x, a); fflush(toklog);
%}

%option header-file="lex.yy.h"
%option noyywrap

%x S_SELECTION

var       [a-zA-Z]+(:[a-zA-Z0-9]+)?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*
ws        [ \t\n\r]
comment   #.*\n

%%

<INITIAL,S_SELECTION>{ws} { LOGV("%s", yytext); }

{comment} { }

composition {
    LOG("COMPOSITION ");
    return COMPOSITION;
}
effect {
    BEGIN(S_SELECTION);
    LOG("EFFECT ");
    return EFFECT;
}
{var} { 
    LOGV("VARIABLE:%s ", yytext);
    yylval.VARIABLE = id_from_str(yytext);
    return VARIABLE;
}
<S_SELECTION>{selection} {
    BEGIN(INITIAL);
    LOGV("SELECTION:%s ", yytext);
    yylval.SELECTION = selection_from_str(yytext);
    return SELECTION;
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
