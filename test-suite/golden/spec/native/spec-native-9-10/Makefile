all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus stream -f > obs.txt 2> obs.err
	echo >> obs.txt
	./nexus writeBags 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
