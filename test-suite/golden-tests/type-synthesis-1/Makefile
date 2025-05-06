all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo 42 > obs.txt
	./nexus foos 42 >> obs.txt
	./nexus sfoos '[42]' >> obs.txt
	./nexus bar 42 >> obs.txt

clean:
	rm -rf nexus* pool*
