all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in viaPlain viaLookalike; do \
	  ./nexus $$c 5 > /dev/null 2> $$c.err; \
	  echo "$$c exit=$$? $$(grep -q 'did not create' $$c.err && echo not-reified || echo OTHER)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *.err __pycache__ .RData
	rm -rf *-build *build.tmp*
