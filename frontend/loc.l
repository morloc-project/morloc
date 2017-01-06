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

%s S_TYPE
%s S_ONTOLOGY
%s S_IMPORT
%s S_EXPORT
%x X_COMMENT
%x X_SOURCE
%s S_LANG
%s S_PATH
%s S_CPLT
%s S_ARG

ws        [ \t\n\r]
comment   #.*\n

sym       [a-zA-Z_][a-zA-Z0-9_]*
var       {sym}(:{sym})?
path      {var}({ws}*\/{ws}*{var})*
selection {path}(,{path})*{ws}*::

identifier {var}{ws}*::

parameter {sym}{ws}*=

grpref     \*{var}
positional `[^`]+`

str \'[^']*\'|\"[^"]*\"
int [0-9]|[1-9][0-9]+
dbl {int}\.[0-9]+
lgc TRUE|FALSE|NULL 

couple ::

%%


{comment} { }

@comment { LOG( "SECTION_COMMENT" ); BEGIN(X_COMMENT); }
@import  { LOG( "SECTION_IMPORT" ); BEGIN(S_IMPORT); }

@source { LOG( "SECTION_SOURCE" ); BEGIN(S_LANG); return SECTION_SOURCE ; }

@path   { LOG( "SECTION_PATH"   ) ; BEGIN(S_PATH) ; return SECTION_PATH   ; }

@type     { LOG( "SECTION_TYPE"     ) ; BEGIN(S_TYPE)     ; return SECTION_TYPE       ; }
@ontology { LOG( "SECTION_ONTOLOGY" ) ; BEGIN(S_ONTOLOGY) ; return SECTION_ONTOLOGY   ; }

@effect { LOG( "SECTION_EFFECT" ) ; BEGIN(S_CPLT) ; return SECTION_EFFECT ; }
@cache  { LOG( "SECTION_CACHE"  ) ; BEGIN(S_CPLT) ; return SECTION_CACHE  ; }
@check  { LOG( "SECTION_CHECK"  ) ; BEGIN(S_CPLT) ; return SECTION_CHECK  ; }
@open   { LOG( "SECTION_OPEN"   ) ; BEGIN(S_CPLT) ; return SECTION_OPEN   ; }
@pack   { LOG( "SECTION_PACK"   ) ; BEGIN(S_CPLT) ; return SECTION_PACK   ; }
@pass   { LOG( "SECTION_PASS"   ) ; BEGIN(S_CPLT) ; return SECTION_PASS   ; }
@fail   { LOG( "SECTION_FAIL"   ) ; BEGIN(S_CPLT) ; return SECTION_FAIL   ; }
@alias  { LOG( "SECTION_ALIAS"  ) ; BEGIN(S_CPLT) ; return SECTION_ALIAS  ; }
@doc    { LOG( "SECTION_DOC"    ) ; BEGIN(S_CPLT) ; return SECTION_DOC    ; }
@arg    { LOG( "SECTION_ARG"    ) ; BEGIN(S_ARG)  ; return SECTION_ARG    ; }

@export { LOG( "SECTION_EXPORT" ); BEGIN(S_EXPORT); return SECTION_EXPORT; }

{ws} { LOGV("%s", yytext); }

<S_LANG>[ ]+{var} {
    LOG(" LANG");
    BEGIN X_SOURCE;
    while(isspace(yytext[0])) yytext++;
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.STR = w;
    return STR;
  }
<X_SOURCE>^[ \t]*([^\n@].*)?\n {
    char* s = strdup(yytext);
    LOGV("%s", s);
    s[strlen(s)-1] = '\0'; /* trim the newline */ 
    W* w = w_new(P_STRING, s);
    yylval.STR = w;
    return STR;
}
<X_SOURCE>@ { unput('@'); BEGIN INITIAL; } /* allow @ anywhere except ^ */


<X_COMMENT>@      { unput('@'); BEGIN INITIAL; }
<X_COMMENT>(.|\n) { /* toss bodies */ }
<X_COMMENT>\\@    { /* allow escaped @ */ }

<S_EXPORT>as { LOG("AS"); return AS; }
<S_EXPORT>{var} {
    LOGV("STRING(%s)", yytext);
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.STR = w;
    return STR;
}

<S_IMPORT>{str} {
    char* s = strdup(yytext);
    s++;
    s[strlen(s)-1]='\0';
    LOGV("importing(%s)\n", s);
    if(!newfile(s)) yyterminate();
  }
<<EOF>> { if(!popfile()) yyterminate(); }

<S_PATH,S_CPLT,S_ARG,S_TYPE,S_ONTOLOGY>{couple} { LOG("COUPLE"); return COUPLE; }

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


<S_CPLT,S_ARG>{selection} {
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

<S_CPLT>{str} {
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.STR = w;
    LOGV("STRING:%s", yytext);
    return STR;
}

<S_ARG>{parameter} {
    char* s = strdup(yytext);
    unput('=');
    for(int i = 0; i < strlen(s); i++){
        s[i] = (isspace(s[i]) || s[i] == '=') ? '\0' : s[i];
    }
    W* w = w_new(P_STRING, s);
    yylval.NAME = w;
    return NAME;
}
<S_ARG>{int}|{dbl}|{lgc}|{sym}|{str} {
    W* w = w_new(P_STRING, strdup(yytext));
    LOG("PRIMITIVE");
    yylval.PRIMITIVE = w;
    return PRIMITIVE;
}

<S_TYPE,S_ONTOLOGY>{sym}{ws}*:: {
    char* s = strdup(yytext);
    unput(':');unput(':');
    for(int i = 0; i < strlen(s); i++){
        s[i] = (isspace(s[i]) || s[i] == ':') ? '\0' : s[i];
    }
    W* w = w_new(P_STRING, s);
    yylval.NAME = w;
    LOG("NAME");
    return NAME;
}
<S_TYPE>{sym} {
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.TYPE = w;
    LOGV("TYPE:%s", yytext);
    return TYPE;
}
<S_TYPE>-> { LOG("->"); return ARROW; }

<S_ONTOLOGY>{sym} {
    W* w = w_new(P_STRING, strdup(yytext));
    yylval.OTYPE = w;
    LOGV("OTYPE:%s", yytext);
    return OTYPE;
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
        type_check(global_table);
        fclose(toklog);
    }
    return status;
}
