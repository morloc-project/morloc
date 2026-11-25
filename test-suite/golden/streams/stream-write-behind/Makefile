W := MORLOC_WRITE_BUFFER_BYTES=65536
N := 200 500

all:
	rm -f *.err *.dat
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	$(W) ./nexus -f packet -z 0 produce $(N) > z0.dat 2>> obs.err
	$(W) ./nexus -f packet -z 3 produce $(N) > z3.dat 2>> obs.err
	$(W) MORLOC_WRITE_BEHIND_DEPTH=0 ./nexus -f packet -z 3 produce $(N) > z3sync.dat 2>> obs.err
	$(W) MORLOC_WRITE_BEHIND_DEPTH=1 ./nexus -f packet -z 3 produce $(N) > z3d1.dat 2>> obs.err
	$(W) ./nexus produceFile f3.dat $(N) 2>> obs.err
	$(W) MORLOC_WRITE_BEHIND_DEPTH=0 ./nexus produceFile f3sync.dat $(N) 2>> obs.err
	for f in z0 z3 z3sync z3d1 f3 f3sync; do printf '%s ' $$f >> obs.txt; ./nexus check $$f.dat $(N) >> obs.txt 2>> obs.err; done
	if test $$(wc -c < z3.dat) -lt $$(wc -c < z0.dat); then echo "OK compressed" >> obs.txt; else echo "UNEXPECTED not compressed" >> obs.txt; fi
	if test $$(wc -c < z3.dat) -eq $$(wc -c < z3sync.dat); then echo "OK same size as synchronous" >> obs.txt; else echo "UNEXPECTED size differs from synchronous" >> obs.txt; fi
	if test $$(wc -c < f3.dat) -eq $$(wc -c < f3sync.dat); then echo "OK file same size as synchronous" >> obs.txt; else echo "UNEXPECTED file size differs from synchronous" >> obs.txt; fi
	rm -f *.dat

clean:
	rm -rf nexus pools __pycache__ *.err *.dat
	rm -rf *-build *build.tmp*
