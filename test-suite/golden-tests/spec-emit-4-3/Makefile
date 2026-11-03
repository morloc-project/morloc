all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  for a in "allIdx [0,1,2,3,4,5,6,7,8,9]" "allIdx [0,1,2,3,4,5,6,7,8,8]" "anyIdx [0,1,2,3,4,5,6,7,8,100]" "anyIdx [0,1,2,3,4,5,6,7,8,9]" "shortCut [0,1,2,3,4,5,6,7,8,9]" "listEq [0,1,2,3,4,5,6,7,8,9]" "listEq [0,1,2,3,4,5,6,7,0,9]" "mixed [0,1,2,3,4,5,6,7,8,9]" "mixed [5,5,5,5,5,5,5,5,5,5]" "scoped [0,1,2]" "scoped [0,2,2]" "condLet [0,1,2]" "condLet [0,1,3]" "flat [true,true]" "flat [true,false]"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
