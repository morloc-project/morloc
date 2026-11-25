all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  for a in "listMod 2 5" "listMod 2 3" "pairMod 1 4" "recEq 1 1" "recEq 1 2" "recNe 1 2" "recLt 1 2" "recLe 2 2" "recGt 1 2" "recGe 2 1" "listRec 1 2" "listRec 2 1" "pairRec 2 1" "pairRec 1 2" "optRec 3 3" "optRec 3 4" "zAgree 0.0 -0.0" "zAgree 1.5 1.5"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
