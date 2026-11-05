all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus eqLit 3 > obs.txt 2> obs.err
	./nexus eqSigned 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
