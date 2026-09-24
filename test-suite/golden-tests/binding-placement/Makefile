all:
	rm -f *.err obs.txt note.log
	morloc make -o nexus main.loc 2> build.err
	echo "--- letGuarded ---" > obs.txt
	-./nexus letGuarded 6 0 >> obs.txt 2>> obs.err
	echo "--- letInBranch ---" >> obs.txt
	-./nexus letInBranch 6 0 >> obs.txt 2>> obs.err
	echo "--- whereInTry ---" >> obs.txt
	-./nexus whereInTry 6 0 >> obs.txt 2>> obs.err
	echo "--- letInTry ---" >> obs.txt
	-./nexus letInTry 6 0 >> obs.txt 2>> obs.err
	echo "--- barrierA ---" >> obs.txt
	-./nexus barrierA 3 0 >> obs.txt 2>> obs.err
	echo "--- barrierB ---" >> obs.txt
	-./nexus barrierB 3 0 >> obs.txt 2>> obs.err
	echo "--- doLetAfterEffect ---" >> obs.txt
	if ./nexus doLetAfterEffect 6 0 > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	cat note.log >> obs.txt

clean:
	rm -rf nexus __pycache__ *.err note.log
	rm -rf *-build *build.tmp*
