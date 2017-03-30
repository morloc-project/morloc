file frontend/morloc

set print repeats 100

set args frontend/test.loc

source gdb-scripts/print.gdb

break -function build_manifolds
