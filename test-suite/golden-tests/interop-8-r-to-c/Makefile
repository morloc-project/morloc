all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo "true" > obs.txt
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
