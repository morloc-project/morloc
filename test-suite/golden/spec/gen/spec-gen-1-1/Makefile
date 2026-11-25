all:
	rm -f *.err obs.txt
	morloc make -o nexus-main main.loc 2> main.err
	morloc make -o nexus-plain plain.loc 2> plain.err
	./nexus-main foo 2 > obs.txt 2> obs.err
	./nexus-plain summary '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err
	rm -rf nexus-* *-build *build.tmp*
