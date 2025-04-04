all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus foo '"alice"' > obs.txt

clean:
	rm -rf pool* nexus* __pycache__
