all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo 1.3 > obs.txt
	diff exp.txt obs.txt

clean:
	rm -f nexus* pool*
