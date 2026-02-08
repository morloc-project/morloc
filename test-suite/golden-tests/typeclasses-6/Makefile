all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '"alice"' 42 > obs.txt

clean:
	rm -rf nexus* pool* __pycache__
