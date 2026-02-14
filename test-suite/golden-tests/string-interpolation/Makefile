all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus sr   '"Alice"' '"Bob"'  > obs.txt 2> obs.err
	./nexus spy  '"Alice"' '"Bob"' 2>> obs.err  >> obs.txt
	./nexus scpp '"Alice"' '"Bob"' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
