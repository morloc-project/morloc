all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	for f in pyF32 rF32 cliF32; do \
	  for v in 1.5 1e39; do \
	    if ./nexus $$f $$v > run.out 2>> obs.err; then echo "$$f $$v ok $$(cat run.out)" >> obs.txt; \
	    else echo "$$f $$v rejected" >> obs.txt; fi; \
	  done; \
	done
	grep -c "out of range for F32" obs.err >> obs.txt
	rm -f run.out

clean:
	rm -rf nexus pools __pycache__ *.err run.out
	rm -rf *-build *build.tmp*
