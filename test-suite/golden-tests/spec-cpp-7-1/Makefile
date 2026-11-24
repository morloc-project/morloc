all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus twice 5 >> obs.txt 2>> obs.err
	./nexus viaParam 5 >> obs.txt 2>> obs.err
	./nexus viaHeader 5 >> obs.txt 2>> obs.err
	./nexus viaLambda 5 >> obs.txt 2>> obs.err
	./nexus scalar 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
