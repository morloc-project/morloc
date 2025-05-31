all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus fastaRevcom '"obs.txt"' '"test.fasta"' > /dev/null

clean:
	rm -f nexus* pool*
