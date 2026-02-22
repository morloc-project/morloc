#!/usr/bin/env bash

set -e

morloc make foo.loc

hyperfine \
  -w 5 \
  -L test pcis,scis \
  --export-markdown cis.markdown \
  --export-csv cis.csv \
  "./nexus {test} [1,2,3,4,5,6]"

hyperfine \
  -w 5 \
  -L test ptrans,strans \
  --export-markdown trans.markdown \
  --export-csv trans.csv \
  "./nexus {test} [1,2,3,4,5,6]"
