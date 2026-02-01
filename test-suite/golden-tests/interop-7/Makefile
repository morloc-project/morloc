all:
	rm -f obs.txt
	morloc make foo.loc 
	./nexus foo 13 > obs.txt
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
