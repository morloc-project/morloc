all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	for f in deadSrc liveSrc deadTake; do for j in 0 1; do ./nexus $$f 5 $$j >> obs.txt 2>> obs.err; done; done

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
