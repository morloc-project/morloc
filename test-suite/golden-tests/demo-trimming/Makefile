all:
	rm -f *.err obs.txt *.err
	morloc make -o nexus foo.loc 2> build.err
	./nexus readFASTQ '"example.fastq"' | \
		./nexus trim config.json /dev/stdin | \
		./nexus writeFASTA '"obs.txt"' /dev/stdin

clean:
	rm -rf nexus __pycache__
