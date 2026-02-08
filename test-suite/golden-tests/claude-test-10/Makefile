all:
	rm -f obs.txt
	morloc make main.loc
	./nexus analyze --sample 1 > obs.txt
	./nexus motifs --sample 1 --kmer-length 3 >> obs.txt
	./nexus align --seq1 1 --seq2 2 >> obs.txt
	./nexus gcContent 1 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
