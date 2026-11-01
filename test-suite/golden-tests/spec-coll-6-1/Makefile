all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus hashes 5 > obs.txt 2> obs.err
	./nexus equal 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
