all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus viaPy 1 > obs.txt 2> obs.err
	./nexus viaR 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
