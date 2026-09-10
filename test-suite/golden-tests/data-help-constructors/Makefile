all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- enum ---" >> obs.txt
	./nexus paint -h >> obs.txt 2>> obs.err
	echo "--- optional enum ---" >> obs.txt
	./nexus shade -h >> obs.txt 2>> obs.err
	echo "--- list of enum ---" >> obs.txt
	./nexus blend -h >> obs.txt 2>> obs.err
	echo "--- variant ---" >> obs.txt
	./nexus measure -h >> obs.txt 2>> obs.err
	echo "--- named option ---" >> obs.txt
	./nexus mix -h >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
