all:
	morloc make main.loc
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus f '"hello"' > obs.txt

clean:
	rm -rf nexus nexus.c pool.py lib/foo/__pycache__
