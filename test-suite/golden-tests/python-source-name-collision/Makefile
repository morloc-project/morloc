all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- fromCopy ---" > obs.txt
	./nexus fromCopy hi >> obs.txt 2>> obs.err
	echo "--- fromTime ---" >> obs.txt
	./nexus fromTime hi >> obs.txt 2>> obs.err
	echo "--- fromStruct ---" >> obs.txt
	./nexus fromStruct hi >> obs.txt 2>> obs.err
	echo "--- fromHeapq ---" >> obs.txt
	./nexus fromHeapq hi >> obs.txt 2>> obs.err
	echo "--- fromE ---" >> obs.txt
	./nexus fromE hi >> obs.txt 2>> obs.err
	echo "--- fromJson ---" >> obs.txt
	./nexus fromJson hi >> obs.txt 2>> obs.err
	echo "--- fromSubCopy ---" >> obs.txt
	./nexus fromSubCopy hi >> obs.txt 2>> obs.err
	echo "--- fromSibling ---" >> obs.txt
	./nexus fromSibling hi >> obs.txt 2>> obs.err
	echo "--- fromPymorloc ---" >> obs.txt
	./nexus fromPymorloc hi >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__ sub/__pycache__
	rm -rf *-build *build.tmp*
