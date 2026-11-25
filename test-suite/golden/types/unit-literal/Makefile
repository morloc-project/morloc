all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- pyUnit ---" >> obs.txt
	./nexus pyUnit 4 >> obs.txt 2>> obs.err
	echo "--- cppUnit ---" >> obs.txt
	./nexus cppUnit 4 >> obs.txt 2>> obs.err
	echo "--- rustUnit ---" >> obs.txt
	./nexus rustUnit 4 >> obs.txt 2>> obs.err
	echo "--- pyGuard ---" >> obs.txt
	./nexus pyGuard 1 >> obs.txt 2>> obs.err
	./nexus pyGuard 0 >> obs.txt 2>> obs.err
	echo "--- cppGuard ---" >> obs.txt
	./nexus cppGuard 1 >> obs.txt 2>> obs.err
	./nexus cppGuard 0 >> obs.txt 2>> obs.err
	echo "--- rustGuard ---" >> obs.txt
	./nexus rustGuard 1 >> obs.txt 2>> obs.err
	./nexus rustGuard 0 >> obs.txt 2>> obs.err
	echo "--- pyBare ---" >> obs.txt
	./nexus pyBare 2 >> obs.txt 2>> obs.err
	echo "--- cppBare ---" >> obs.txt
	./nexus cppBare 2 >> obs.txt 2>> obs.err
	echo "--- rustBare ---" >> obs.txt
	./nexus rustBare 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
