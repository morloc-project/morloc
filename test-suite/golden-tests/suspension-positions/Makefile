all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in letReuse recordField listElement viaId constant argumentOnce nested nestedIdentity constructed pure; do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.txt; done

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
