all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- plainState ---" > obs.txt
	./nexus plainState hi >> obs.txt 2>> obs.err
	echo "--- appState ---" >> obs.txt
	./nexus appState hi >> obs.txt 2>> obs.err
	echo "--- appCopy ---" >> obs.txt
	./nexus appCopy hi >> obs.txt 2>> obs.err
	echo "--- rel ---" >> obs.txt
	./nexus rel hi >> obs.txt 2>> obs.err
	echo "--- nsLeaf ---" >> obs.txt
	./nexus nsLeaf hi >> obs.txt 2>> obs.err

clean:
	rm -rf nexus *.err __pycache__ app/__pycache__ ns/__pycache__
	rm -rf *-build *build.tmp*
