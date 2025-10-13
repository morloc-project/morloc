all:
	morloc make main.loc > log
	./nexus sr   '"Alice"' '"Bob"'  > obs.txt
	./nexus spy  '"Alice"' '"Bob"' >> obs.txt
	./nexus scpp '"Alice"' '"Bob"' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
