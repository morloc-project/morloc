all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus s1 > obs.txt 2> obs.err
	./nexus s2 >> obs.txt 2>> obs.err
	./nexus s3 >> obs.txt 2>> obs.err
	./nexus s4 >> obs.txt 2>> obs.err
	./nexus s5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
