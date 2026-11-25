all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	for f in lastUse reused byRef both; do echo "== $$f" >> obs.txt; ./nexus $$f 3 >> obs.txt 2>> obs.err; done
	echo "== views" >> obs.txt
	./nexus views abcd '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
