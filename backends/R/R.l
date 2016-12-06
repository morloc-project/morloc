%{
    #include "R.tab.h"
%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

emit    ^emit
link    ^link
ws      [ \t]+
par     [a-zA-Z_.][a-zA-Z0-9_.]*
eol     [\r\n]+

%%

{ws}      { }
{emit}    { return EMIT; }
{link}    { return LINK; }
{par}     { yylval = strdup(yytext); return PAR; }
{eol}     { return EOL; }
