all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus reals 0 >> obs.txt 2>> obs.err
	./nexus echoReals "$$(./nexus reals 0)" >> obs.txt 2>> obs.err
	./nexus echoF32s '[7.038531e-26,16777215,0.33333334,3.4028235e38,0.1,1e-45]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
