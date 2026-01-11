all:
	rm -f obs.txt
	morloc make main.loc > log
	./nexus foo 33 > obs.txt

clean:
	rm -rf pool* nexus* __pycache__
