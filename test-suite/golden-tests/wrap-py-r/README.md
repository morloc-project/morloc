# module creation

This is a test of morloc's ability to generate an R module that can call
everything defined in a morloc script. 

`foo.loc` exports three simple functions.

 * `dec` - an R function, this should be exported from the ultimate R package as an unwrapped R function
 * `inc` - python function, this should be a foreign call from R
 * `fac` - python math.factorial function, this should be a foreign call from R

For now foreign calls all involve serialization, but this will change in the future.

Building an R package requires the following lines:

```
Rscript -e "devtools::create("rfoo")'
mv rfoo.R rfoo/R
Rscript -e "devtools::install("rfoo")'
```

Uninstalling the package requires:

```
Rscript -e 'remove.packages("rfoo")'
```
