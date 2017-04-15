# Clustering in R and Python

The machine learning and statistics communities heavily overlap, but the former
mostly uses Python and the latter R. Many bleeding-edge machine learning
algorithms are available in Python libraries such as `scikit-learn`. R has
a ridiculously comprehensive statistical and data-analytic library, much of
which is missing in Python.

There is a well-earned aversion to multi-lingual programming. There are good
reasons for this. Learning new languages is a huge investment. Every new
language you add a new layer of interfaces between it and each existing
language.

Here are a few approaches to linking R and Python

 1. Embed both languages in a scripting language, e.g. Bash
    Pros
      * no extra dependencies
      * low learning curve (assuming you know Bash, R, and Python). No new
        libraries to learn and code can be idiomatic in each script.
    Cons
      * adds a third language
      * requires writing both Bash and Python languages as independent
        executables and handling their arguments, inputs and outputs
      * slow, since this requires repeatedly loading the interpreter

 2. Invoke the foreign language using a system call. 
    Pros
      * uses just two languages
    Cons
      * systems calls a ugly (and very different) in both languages
      * can get tricky problems with cross-calls
      * slow, since this requires repeatedly loading the interpreter

 3. For Python to R, use a special library `rpy2`.
    Pros
      * very fast, since Python sends data to a embeded instance of the
        R interpreter
    Cons
      * verbose and non-idiomatic, since every R function needs to be wrapped
        in Python code
      * only works one way, there isn't a similar program that allows Python to
        be run from inside R.

 4. Morloc
    Pros
      * no boilerplate
      * no extra IO code
      * completely idiomatic
    Cons
      * third language problem
      * Foreign calls to interpreted languages are slow currently. But this is
        a compiler problem, future Morlock implementations could use existing
        libraries for linking languages in its generated code.
