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
%option noyywrap pointer yylineno

%s S_IMPORT
%s S_PATH
%s S_CPLT

ws        [ \t\n\r]
comment   #.*\n

name      [a-zA-Z_][a-zA-Z0-9_]*
var       {name}(:{name})?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*{ws}*::

identifier {var}{ws}*::

grpref     \*{var}
positional `[^`]+`

string \'[^']*\'|\"[^"]*\"

couple ::

%%


    /* comment  */
    /* alias    */
    /* arg      */
    /* export   */
    /* ontology */
    /* type     */
    /* source   */

^@import { LOG( "SECTION_IMPORT" ); BEGIN(S_IMPORT); }

^@path   { LOG( "SECTION_PATH"   ) ; BEGIN(S_PATH) ; return SECTION_PATH   ; }

^@effect { LOG( "SECTION_EFFECT" ) ; BEGIN(S_CPLT) ; return SECTION_EFFECT ; }
^@cache  { LOG( "SECTION_CACHE"  ) ; BEGIN(S_CPLT) ; return SECTION_CACHE  ; }
^@check  { LOG( "SECTION_CHECK"  ) ; BEGIN(S_CPLT) ; return SECTION_CHECK  ; }
^@open   { LOG( "SECTION_OPEN"   ) ; BEGIN(S_CPLT) ; return SECTION_OPEN   ; }
^@pack   { LOG( "SECTION_PACK"   ) ; BEGIN(S_CPLT) ; return SECTION_PACK   ; }
^@pass   { LOG( "SECTION_PASS"   ) ; BEGIN(S_CPLT) ; return SECTION_PASS   ; }
^@fail   { LOG( "SECTION_FAIL"   ) ; BEGIN(S_CPLT) ; return SECTION_FAIL   ; }
^@doc    { LOG( "SECTION_DOC"    ) ; BEGIN(S_CPLT) ; return SECTION_DOC    ; }

<S_IMPORT>{string} {
    char* s = strdup(yytext);
    s++;
    s[strlen(s)-1]='\0';
    LOGV("importing(%s)\n", s);
    if(!newfile(s)) yyterminate();
  }
<<EOF>> { if(!popfile()) yyterminate(); }

{ws} { LOGV("%s", yytext); }

{comment} { }

<S_PATH,S_CPLT>{couple} { LOG("COUPLE"); return COUPLE; }

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
    W* w = label_from_str(s);
    LOGV("IDENTIFIER(%s,", w->value.label->name);
    LOGV("%s) ", w->value.label->label);
    yylval.IDENTIFIER = w;
    return IDENTIFIER;
}


<S_PATH>{var} {
    Manifold* m = manifold_new();
    W* l = label_from_str(yytext);
    W* r = w_new(P_MANIFOLD, m);
    Couplet* c = couplet_new(l, r); 
    W* wc = w_new(C_MANIFOLD, c);
    LOG("C_MANIFOLD");
    yylval.COMPOSON = wc;
    return COMPOSON;
}
<S_PATH>{grpref} {
    char* s = strdup(yytext + 1);
    W* w = w_new(C_GRPREF, s);
    yylval.COMPOSON = w;
    LOG("C_GRPREF");
    return COMPOSON;
}
<S_PATH>{positional} {
    // copy the section of the string between the quotation marks
    int N = strlen(yytext);
    char* s = (char*)calloc((N-1), sizeof(char));
    memcpy(s, yytext+1, (N-2)*sizeof(char));
    s[N-2] = '\0';
    W* w = w_new(C_POSITIONAL, s);
    yylval.COMPOSON = w;
    LOG("C_POSITIONAL");
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

    W* w = list_from_str(s);
    yylval.SELECTION = w;
    LOG("SELECTION");
    return SELECTION;
}

<S_CPLT,S_PATH>{var} {
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.VARIABLE = w;
    LOGV("VARIABLE:%s", yytext);
    return VARIABLE;
}

<S_CPLT>{string} {
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.STRING = w;
    LOG("STRING");
    return STRING;
}

. { LOGV("%c", yytext[0]); return yytext[0]; }

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
