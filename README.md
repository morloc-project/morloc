# Rats

A typed dataflow language that supports loops and conditionals

# Examples

The example below shows my current vision, little of this has been implemented

```
{ (A:1 --> B) }:L1
{ (A:2 --> C) }:L2 --> D --> { E }:L3

alias:
A = senhalatha
B = huzurbazar
C = fredericksen
D = alonganduninterestingname
E = thelastfunctioninthisparticularpipeline

type:
A :: NIL -> a
B :: a -> b
C :: a -> c
D :: f -> g -> h
E :: h -> i

arg:
# I have to label the two A's since I am treating them differently, so they
# need their own caches, etc.
A:1 :: x=45, y="hi", z=[1,2,3]
A:2 :: x=46

val:
B --> va
C --> va
D --> vb, vc
A B C --> vabc

eff:
A --> ea
B --> eb
C --> ec
A D --> ead
ea eb ec ead --> final
vabc --> log

groups:
mostofthem = B,C,D,E

cache:
# using the group label biguns is same as doing:
# B,C,D,E --> datcache
mostofthem --> memcache
A --> nocache
# turn on memcache for all effectors
__eff__ --> memcache
# overide the previous, so final has datacache
final --> datcache

fail:
__all__ --> fail

pass:
__map__ --> execute

L1:
# Since I am splitting by parameter, I needn't specify S
M :: mergefunction
A:1<i> :: a=[1,2,3,4]
B<i>   :: x=[1,2]     # recyle 1 and 2 -- 4 threads

L2:
# Since I am splitting by parameter, I needn't specify S
M :: mergefunction
A:2<i> :: b=[2,3,9]
C<j> :: y=[1,2]     # take all permutations of i,j -- 6 threads

L3:
S :: cut_input_into_chunks
M :: merge_processed_chunks

```
