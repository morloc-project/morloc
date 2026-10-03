all:
	rm -f *.err killed-dirs.txt
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./run.sh default "" >> obs.txt 2>> obs.err
	./run.sh thread thread >> obs.txt 2>> obs.err
	./swept.sh >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err started.txt killed-dirs.txt
	rm -rf *-build *build.tmp*
