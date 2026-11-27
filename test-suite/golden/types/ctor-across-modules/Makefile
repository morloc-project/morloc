all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- fromMid ---" >> obs.txt
	./nexus fromMid >> obs.txt 2>> obs.err
	echo "--- matchAlias ---" >> obs.txt
	./nexus matchAlias Red >> obs.txt 2>> obs.err
	./nexus matchAlias Blue >> obs.txt 2>> obs.err
	echo "--- selective ---" >> obs.txt
	./nexus selective 1.5 >> obs.txt 2>> obs.err
	echo "--- roundtrip ---" >> obs.txt
	./nexus roundtrip >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
