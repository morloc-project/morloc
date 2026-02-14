all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus f '"hello"' > obs.txt 2> obs.err

clean:
	rm -rf nexus nexus.c pool.py lib/foo/__pycache__
