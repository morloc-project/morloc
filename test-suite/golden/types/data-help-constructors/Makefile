all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- enum ---" >> obs.txt
	./nexus paint -hhh >> obs.txt 2>> obs.err
	echo "--- optional enum ---" >> obs.txt
	./nexus shade -hhh >> obs.txt 2>> obs.err
	echo "--- list of enum ---" >> obs.txt
	./nexus blend -hhh >> obs.txt 2>> obs.err
	echo "--- variant ---" >> obs.txt
	./nexus measure -hhh >> obs.txt 2>> obs.err
	echo "--- named option ---" >> obs.txt
	./nexus mix -hhh >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
