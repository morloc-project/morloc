all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus localBody 5 > obs.txt 2> obs.err
	./nexus foreignBody 5 >> obs.txt 2>> obs.err
	./nexus ignoresAll 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
