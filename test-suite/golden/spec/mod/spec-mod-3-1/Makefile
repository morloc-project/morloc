all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus inc 1 2>> obs.err >> obs.txt
	./nexus twice 1 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
	rm -f *.err
