%{
    #include "rats.tab.h"
%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

comment #.*|\/\/.*
ws      [ \t]*
var     [a-zA-Z_][a-zA-Z0-9_]*
dep     -->
eol     [\r\n]+

%%

{comment} { }
{ws}      { }
{var}     { yylval = yytext; return VAR; }
{dep}     { return DEP; }
{eol}     { return EOL; }
