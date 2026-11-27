all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus ints 3 >> obs.txt 2>> obs.err
	./nexus reals 3 >> obs.txt 2>> obs.err
	./nexus bools 3 >> obs.txt 2>> obs.err
	./nexus ints 20000 2>> obs.err | tr -d '[]' | tr ',' ' ' | wc -w | tr -d ' ' >> obs.txt
	./nexus reals 20000 2>> obs.err | tr -d '[]' | tr ',' ' ' | wc -w | tr -d ' ' >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
