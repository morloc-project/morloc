all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo 6 5 > obs.txt
	./nexus.py bar '"a"' '"b"' >> obs.txt

clean:
	rm -f nexus* pool*
