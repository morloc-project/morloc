all:
	morloc make main.loc 

clean:
	rm -rf nexus.py pool.cpp *out */nexus.py */*out */pool.cpp revcom.fasta

run:
	./nexus.py fastaRevcom '"revcom.fasta"' '"test.fasta"' && cat revcom.fasta
