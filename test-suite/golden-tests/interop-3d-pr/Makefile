all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo 5 [1,2,3] > obs.txt

clean:
	rm -f nexus* pool* *pdf
