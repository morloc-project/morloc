all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py foo 42 > obs.txt
	./nexus.py foos 42 >> obs.txt
	./nexus.py sfoos '[42]' >> obs.txt
	./nexus.py bar 42 >> obs.txt

clean:
	rm -rf nexus* pool*
