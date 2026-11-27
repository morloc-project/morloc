all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in poolList nexusList poolTuple nexusTuple poolBox nexusBox poolFn nexusFn; do ./nexus $$c 1 >> obs.txt 2>> obs.err; done

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
