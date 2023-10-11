all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo "[1.0,2.0,3.0]" > obs.txt

clean:
	rm -f nexus* pool*
