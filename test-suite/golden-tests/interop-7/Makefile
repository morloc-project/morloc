all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	./nexus foo 13 > obs.txt
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
