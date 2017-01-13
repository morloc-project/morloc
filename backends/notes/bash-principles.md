# Grammar

A bit out of date, see bash-grammar.m4

```


PROLOGUE ---> `'

MANIFOLD_$1 ------> MANIFOLD($1)
            `.
              `---> FOREIGN_MANIFOLD($1)

MANIFOLD(id) ---> $1() { CACHE_$1 RETURN }

FOREIGN_MANIFOLD(ID) ---> $1(){ CACHE_$1 RETURN }

RETURN ---> `'

CACHE_$1 ------> DO_CACHE($1)
         `.
           `---> NO_CACHE($1)

DO_CACHE(id) ---> if CACHE_CHK_$1 $1
                  then
                      CACHE_GET_$1 $1
                  else
                      VALIDATE_$1
                  fi

VALIDATE_$1 ------> DO_VALIDATE($1)
            `.
              `---> NO_VALIDATE($1)

DO_VALIDATE(id) ---> if CHECK_$1
                     then
                         CORE($1)
                     else
                         FAIL_$1$2
                     fi

NO_VALIDATE(id) ---> CORE($1)

NO_CACHE(id) ---> VALIDATE_$1
                  HOOK_$1

CORE(id) ---> PASS_$1 FUNC_$1 ARG_$1 INPUT_$1
              EFFECT_$1
              PACK_$1 CACHE_PUT_$1

INPUT(id) ---> <($1 OPEN_$1) -----> <($1 | o1)
                             `.
                               `--> <($1)

EPILOGUE ---> if manifold_exists $1
              then
                  $1
              else
                  exit 1 
              fi
```
