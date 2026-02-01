all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo '"alice"' > obs.txt

clean:
	rm -rf pool* nexus* __pycache__
