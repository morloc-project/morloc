REJECT = numpyScalar fakeArray maskedArray missingKey growing shrinking

all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus main.loc 2> build.err
	for f in $(REJECT); do \
	  if ./nexus $$f 3 > run.out 2>> obs.err; then echo "$$f UNEXPECTED $$(cat run.out)" >> obs.txt; \
	  else echo "$$f rejected" >> obs.txt; fi; \
	done
	./nexus numpyArray 3 >> obs.txt 2>> obs.err
	./nexus numpySubclass 3 >> obs.txt 2>> obs.err
	./nexus numpyAfterStrs strs.dat >> obs.txt 2>> obs.err
	./nexus numpyAfterRecords records.dat >> obs.txt 2>> obs.err
	rm -f run.out *.dat

clean:
	rm -rf nexus pools __pycache__ *.err *.dat run.out
	rm -rf *-build *build.tmp*
