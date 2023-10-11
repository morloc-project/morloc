all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo 2.4 >> obs.txt

clean:
	rm -f nexus* pool*
