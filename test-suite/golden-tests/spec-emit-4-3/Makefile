all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  for a in "allIdx [0,1,2,3,4,5,6,7,8,9]" "allIdx [0,1,2,3,4,5,6,7,8,8]" "anyIdx [0,1,2,3,4,5,6,7,8,100]" "anyIdx [0,1,2,3,4,5,6,7,8,9]" "shortCut [0,1,2,3,4,5,6,7,8,9]" "listEq [0,1,2,3,4,5,6,7,8,9]" "listEq [0,1,2,3,4,5,6,7,0,9]" "mixed [0,1,2,3,4,5,6,7,8,9]" "mixed [5,5,5,5,5,5,5,5,5,5]" "scoped [0,1,2]" "scoped [0,2,2]" "condLet [0,1,2]" "condLet [0,1,3]" "flat [true,true]" "flat [true,false]" "andAt 1" "andAt 8" "andAt 9" "andAt 16" "andAt 17" "andAt 24" "andAt 25" "andAt 32" "andAt 33" "andAt 40" "andAt 41" "orAt 1" "orAt 8" "orAt 9" "orAt 16" "orAt 17" "orAt 24" "orAt 25" "orAt 32" "orAt 33" "orAt 40" "orAt 41"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done
	if awk '{ n = gsub(/&&|[|][|]/, ""); if (n > m) m = n } END { exit !(m <= 15) }' nexus-r-build/pools/r/pool.R; then echo "r runs <= 16: OK" >> obs.txt; else echo "r runs <= 16: UNEXPECTED" >> obs.txt; fi

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
