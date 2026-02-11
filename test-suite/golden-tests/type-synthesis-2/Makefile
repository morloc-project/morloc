all:
	morloc make main.loc 2> build.err
	rm -f obs.txt
	./nexus f '"hello"' > obs.txt 2> obs.err

clean:
	rm -rf nexus nexus.c pool.py lib/foo/__pycache__
