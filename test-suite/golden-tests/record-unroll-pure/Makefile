all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- defaults ---" >> obs.txt
	./nexus same >> obs.txt 2>> obs.err
	echo "--- given ---" >> obs.txt
	./nexus same -n y --size 1 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
