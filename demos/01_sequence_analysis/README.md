# `morloc` DNA sequence example

Before running this example, be sure you have followed the installation
instructions in the top-level README. Then install the `cppbase` morloc module
with `morloc install cppbase`.

First open the `main.loc` file to see the top-level `morloc` program this we
will build. Then build the project as follows:

``` sh
morloc make main.loc
```

This should generate the following files:
 * pool.cpp - the generate C++ source code for (de)serialization and function composition 
 * pool-cpp.out - the compiled code
 * nexus.pl - the user interface

Feel free to skim the `pool.cpp` file to see what the morlocks are up to
underground. You can also read the `nexus.pl` script (which is just a Perl
script). Both of these files are generated based on the `morloc` template
`main.loc`.

To access the usage statement, run `./nexus.pl -h`. This will list all exported
commands and the types of their input and output. Currently the help statement
is pretty minimal, but I'll remedy this in the near future.

Commands can be called as follows:

``` sh
$ ./nexus.pl fasta_revcom '"test.fasta"'
">Unicorn
TGTATCTGTATCTGTATCTGTATC
>Dragon
TGTATCTGTATCTGTATCTGTATCTGTATCTGTATCTGTATCTGTATC
"
```

Why the weird quoting? All inputs to a morloc program are (for now) raw JSON
data. Raw string inputs need two levels of quotation since one level is removed
by Bash (hence, '"test.fasta"'). The returned value is also a JSON string, so
it is quoted. The `write_fasta` function could alternatively be written to
print directly to STDOUT instead of returning a string.

To learn more about module construction, visit the `bio` and `fasta` modules in
this folder.
