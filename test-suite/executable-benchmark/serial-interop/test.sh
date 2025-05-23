#!/usr/bin/env bash

set -e

morloc make foo.loc

# warmup
hyperfine \
  -w 10 \
  -L test pTenBaseline,rTenBaseline,cTenBaseline \
  "./nexus {test}"

hyperfine \
  -w 5 \
  -L test cZeroBaseline,pZeroBaseline,pZeroFromForeign,pZeroToForeign,rZeroBaseline,rZeroFromForeign,rZeroToForeign,cTenBaseline,pTenBaseline,pTenFromForeign,pTenToForeign,rTenBaseline,rTenFromForeign,rTenToForeign,rMarginalCost1,rMarginalCost2,rMarginalCost3,rMarginalCost4,pMarginalCost1,pMarginalCost2,pMarginalCost3,pMarginalCost4 \
  --export-markdown stats.markdown \
  --export-csv stats.csv \
  "./nexus {test}"

hyperfine -w 5 -L test memtest "./nexus {test} medium-list.json"

hyperfine -w 5 -L test mapManyPCP,mapManyPCR "./nexus {test} 5 long-list.json"
