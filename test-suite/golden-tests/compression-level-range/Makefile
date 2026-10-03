FNS = writeNexus saveNexus writeCpp saveCpp writePy savePy writeR saveR writeRust saveRust

all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus main.loc 2> build.err
	for f in $(FNS); do \
	  for l in 9 256 -1; do \
	    if ./nexus $$f -- $$l $$f.dat > run.out 2>> obs.err; then \
	      echo "$$f $$l ok $$(cat run.out)" >> obs.txt; \
	    else echo "$$f $$l rejected" >> obs.txt; fi; \
	    rm -f $$f.dat; \
	  done; \
	done
	grep -c "got -1$$" obs.err >> obs.txt
	rm -f run.out

clean:
	rm -rf nexus pools __pycache__ *.err *.dat run.out
	rm -rf *-build *build.tmp*
