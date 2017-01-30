SEP = {'R',', '}
BND = {'R','='}
AND = {'R', ' && '}

FUNCTION = {
'R':
'''
{mid} = function ({marg}){
    {hook0}
    {cache}
    {hook1}
}
'''
}

HAS_CACHE = {
'R':
'''
if(cache_chk("{mid}")){
    {hook8}
    b = cache_get("{mid}")
    {hook9}
}
else{
    {hook2}
    {validate}
    {hook3}
}
'''
}

NO_CACHE = {
'R':
'''
{hook2}
{validate}
{hook3}
'''
}

HAS_VALIDATE = {
'R':
'''
if( {checks} ){
    {hook4}
    b = {fun}({arguments})
    cache_put("{mid}", b)
    {hook5}
} else {
    {hook6}
    b = fail({margs})
    cache_put("{mid}", b)
    {hook7}
}
'''
}

NO_VALIDATE = {
'R':
'''
{hook4}
b = {fun}({arguments})
cache_put("{mid}", b)
{hook5}
'''
}

ARGUMENTS = {
'R','{inputs}{sep}{fargs}'
}

MANIFOLD_CALL = {
'R', '{mid}({margs})'
}

CHECK_CALL = {
'R', '{mid}({margs})'
}

HOOK = {
'R':'{mid}({margs})'
}
