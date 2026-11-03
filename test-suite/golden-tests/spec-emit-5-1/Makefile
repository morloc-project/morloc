all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  for a in "plain 7" "plain 199" "plain 500" "stmts [0,1,2,3,4,5,6,7,8,9]" "stmts [5,5,5,5,5,5,5,5,5,5]" "stmts [1000,1000,1000,1000,1000,1000,1000,1000,1000,1000]" "countdown 199 0" "countdown 150 7" "shared [5]" "shared [500]" "countdown 7 0" "named 7" "named 13" "named 999" "nested 7" "nested 122" "nested 105"; do \
	    printf '%s %s: ' $$l "$$a" >> obs.txt; ./nexus-$$l $$a >> obs.txt 2>> obs.err || echo "failed" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
