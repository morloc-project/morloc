all:
	morloc make main.loc 2> build.err
	./nexus test > obs.txt 2> obs.err

clean:
	rm -rf __pycache__ pool* nexus* log
