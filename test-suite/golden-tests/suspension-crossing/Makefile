all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in heldValue expression carried forced; do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.txt; done
	echo "--- sink ---" >> obs.txt
	./nexus sink 2 >> obs.txt 2>> obs.txt
	echo "--- sinkCpp ---" >> obs.txt
	./nexus sinkCpp 2 >> obs.txt 2>> obs.txt
	for c in inRecord inOptional absent; do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.txt; done

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
