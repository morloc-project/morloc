all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo 42 > obs.txt
	./nexus.pl foos 42 >> obs.txt
	./nexus.pl sfoos '[42]' >> obs.txt
	./nexus.pl bar 42 >> obs.txt

clean:
	rm -rf nexus* pool*
