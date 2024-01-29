all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo '"a"' '"b"' > obs.txt
	./nexus.py bar 6 5 >> obs.txt

clean:
	rm -f nexus* pool*
