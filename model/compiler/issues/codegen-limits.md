# Limits of generated code

Generated code that a language's own tools refuse.

- [reproduced] R's parser refuses nesting past about 50 levels
  (`contextstack overflow`): 50 nested `if`/`else` blocks fail, where the
  same logic written as `if .. else if ..` parses at any length. morloc
  writes a guard chain (`? c1 = .. : ? c2 = .. : ..`) as nested `if`/`else`
  blocks, so a chain of about 50 cases fails to load in an R pool. Fix:
  write it as `else if` where a case's condition needs no statements of
  its own. Spec gap: no item bounds the nesting of generated code
  (EMIT-4 covers `&&`/`||` chains only).
