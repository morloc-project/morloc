SEP              = {}
BIND             = {}
AND              = {}
POOL             = {}
NATIVE_MANIFOLD  = {}
FOREIGN_MANIFOLD = {}
CACHE            = {}
PROCESS          = {}
DO_VALIDATE      = {}
NO_VALIDATE      = {}
ARGUMENTS        = {}
MANIFOLD_CALL    = {}
CHECK_CALL       = {}
HOOK             = {}
INDENT           = {}
CACHE_PUT        = {}
MARG             = {}
LIST             = {}



# === R grammar ============================================

INDENT['R'] = 2
SEP['R'] = ', '
BIND['R'] = '='
AND['R'] = ' && '
LIST['R'] = 'list({values})'

POOL['R'] = '''\
#!/usr/bin/Rscript --vanilla
library(readr)

{source}

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f = get(m)
  d <- f()
  if(is.data.frame(d)){{
      write_tsv(d, path="/dev/stdout")
  }} else {{
      write_lines(d, path="/dev/stdout")
  }}
}} else {{
  quit(status=1)
}}'''

NATIVE_MANIFOLD['R'] = '''\
{mid} = function ({marg}){{
  {hook0}
{block}
  {hook1}
}}
'''

FOREIGN_MANIFOLD['R'] = '''\
{mid} <- function({marg}){{
  d <- system("{outdir}/call.{lang} {mid} {marg}", intern = TRUE)
  d <- read_tsv(d)
  if(ncol(d) == 1){{
    d <- d[[1]]
  }}
  d
}}
'''

CACHE['R'] = '''\
if({cache}_chk("{mid}")){{
  {hook8}
  b = {cache}_get("{mid}")
  {hook9}
}}
else{{
{process}
}}
'''

PROCESS['R'] = '''\
{hook2}
{validate}
{hook3}
'''

DO_VALIDATE['R'] = '''\
if( {checks} ){{
  {hook4}
  b = {function}({arguments})
  {cache_put}
  {hook5}
}} else {{
  {hook6}
  b = fail({margs})
  {cache_put}
  {hook7}
}}
'''

NO_VALIDATE['R'] = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
'''

CACHE_PUT['R'] = '''\
{cache}_put("{mid}", b)
'''

MARG['R']          = 'x{i}'
ARGUMENTS['R']     = '{inputs}{sep}{fargs}'
MANIFOLD_CALL['R'] = '{hmid}({margs})'
CHECK_CALL['R']    = '{hmid}({margs})'
HOOK['R']          = '{hmid}({margs})'





# === sh grammar ===========================================

INDENT['sh'] = 4
SEP['sh'] = ' '
BIND['sh'] = ' '
AND['sh'] = ' && '
LIST['sh'] = ' {value} '

POOL['sh'] = '''\
#!/usr/bin/env bash

{source}

to_stderr () {{
    $@ 1>&2
}}

{manifolds}

manifold_exists() {{
    type $1 | grep -q function
}}
if manifold_exists $1
then
    if [[ -f $1_tmp ]]
    then
        cat $1_tmp
    else
        $@
    fi
else
    exit 1 
'''

NATIVE_MANIFOLD['sh'] = '''\
{mid} ({marg}) {{
    {hook0}
{block}
    {hook1}
}}
'''

FOREIGN_MANIFOLD['sh'] = '''\
{mid} () {{
    {outdir}/call.{lang} {mid} {marg}
}}
'''

CACHE['sh'] = '''\
if {base_cache}_chk {mid} {uid_arg}
then
    {hook8}
    {base_cache}_get {mid} {uid_arg}
    {hook9}
else
    {process}
    ( cat {mid}_tmp ; rm {mid}_tmp )
fi
'''

PROCESS['sh'] = '''\
{hook2}
{validate}
{hook3}
'''

DO_VALIDATE['sh'] = '''\
if {checks}
then
    {hook4}
    {function} {arguments} > {mid}_tmp
    {cache_put}
    {hook5}
else
    {hook6}
    fail {margs} > {mid}_tmp
    {cache_put}
    {hook7}
fi
'''

NO_VALIDATE['sh'] = '''\
{hook4}
{function} {arguments} > {mid}_tmp
{cache_put}
{hook5}
'''

CACHE_PUT['sh'] = '''\
{cache}_put {mid} b
'''

MARG['sh']          = '${i}'
ARGUMENTS['sh']     = '{inputs} {fargs}'
MANIFOLD_CALL['sh'] = '<({hmid} {margs})'
CHECK_CALL['sh']    = '{hmid} {margs}'
HOOK['sh']          = 'to_stderr {hmid} {margs}'
