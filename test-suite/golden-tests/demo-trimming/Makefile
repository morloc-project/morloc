all:
	rm -f obs.txt *.err
	morloc make foo.loc 2> build.err
	./nexus readFASTQ '"example.fastq"' | \
		./nexus trim config.json /dev/stdin | \
		./nexus writeFASTA '"obs.txt"' /dev/stdin

clean:
	rm -rf nexus* pool* __pycache__
