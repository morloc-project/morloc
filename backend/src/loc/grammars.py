SEP              = {}
BND              = {}
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



# === R grammar ============================================

INDENT['R'] = 2
SEP['R'] = ', '
BND['R'] = '='
AND['R'] = ' && '

POOL['R'] = '''\
#!/usr/bin/Rscript --vanilla
library(readr)

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f = get(m)
  d <- do.call(f, list(args[-1]))
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
if(cache_chk("{mid}")){{
  {hook8}
  b = cache_get("{mid}")
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
  b = {fun}({arguments})
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
b = {fun}({arguments})
{cache_put}
{hook5}
'''

CACHE_PUT['R'] = 'cache_put("{mid}", b)'

MARG['R']          = 'x{i}'
ARGUMENTS['R']     = '{inputs}{sep}{fargs}'
MANIFOLD_CALL['R'] = '{mid}({margs})'
CHECK_CALL['R']    = '{mid}({margs})'
HOOK['R']          = '{mid}({margs})'





# === sh grammar ===========================================

INDENT['sh'] = 4
SEP['sh'] = ' '
BND['sh'] = ' '
AND['sh'] = ' && '

POOL['sh'] = '''\
#!/usr/bin/env bash

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
    {fun} {arguments} > {mid}_tmp
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
{function} {arguments} {inputs} > {mid}_tmp
{cache_put}
{hook5}
'''

CACHE_PUT['sh'] = 'cache_put {mid} b'

MARG['sh']          = '${i}'
ARGUMENTS['sh']     = '{inputs} {fargs}'
MANIFOLD_CALL['sh'] = '<({mid} {margs})'
CHECK_CALL['sh']    = '{mid} {margs}'
HOOK['sh']          = 'to_stderr {mid} {margs}'
