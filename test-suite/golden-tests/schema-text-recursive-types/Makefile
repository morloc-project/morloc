all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	python3 schemas.py >> obs.txt 2>> obs.err
	./nexus nested '{"value":{"value":1,"children":[{"value":2,"children":[]}]},"children":[{"value":{"value":3,"children":[]},"children":[]}]}' >> obs.txt 2>> obs.err
	./nexus alt '{"ANode":[1,{"BNode":[2,{"ANode":[3,"BLeaf"]}]}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
