all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo "true" > obs.txt
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
