#!/usr/bin/env bash

set -e

morloc make foo.loc

hyperfine \
  -w 5 \
  -L test pZeroBaseline,rZeroBaseline,cZeroBaseline,pTenBaseline,rTenBaseline,cTenBaseline,pZeroToForeign,pZeroFromForeign,rZeroToForeign,rZeroFromForeign,pTenToForeign,pTenFromForeign,rTenToForeign,rTenFromForeign,rMarginalCost1,rMarginalCost2,rMarginalCost3,rMarginalCost4,pMarginalCost1,pMarginalCost2,pMarginalCost3,pMarginalCost4 \
  --export-markdown stats.markdown \
  --export-csv stats.csv \
  "./nexus.py {test}"
