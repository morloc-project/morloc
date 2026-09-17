all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus capturedCount 10 >> obs.txt 2>> obs.err
	./nexus capturedCount 100000 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=100000000 ./nexus capturedCount 100000 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./nexus capturedCount 100000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
