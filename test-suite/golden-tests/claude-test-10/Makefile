all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus analyze --sample 1 > obs.txt 2> obs.err
	./nexus motifs --sample 1 --kmer-length 3 2>> obs.err  >> obs.txt
	./nexus align --seq1 1 --seq2 2 2>> obs.err  >> obs.txt
	./nexus gcContent 1 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
