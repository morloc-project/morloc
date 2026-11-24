all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus g 1.5 > obs.txt 2> obs.err
	./nexus k 2 >> obs.txt 2>> obs.err
	./nexus f 2 >> obs.txt 2>> obs.err || echo refused >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
