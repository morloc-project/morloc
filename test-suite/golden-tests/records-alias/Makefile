all:
	rm -f obs.txt
	morloc make foo.loc
	./nexus test 42 > obs.txt

clean:
	rm -rf pool* nexus* __pycache__
