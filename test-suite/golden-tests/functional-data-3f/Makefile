all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus foo true 3 > obs.txt
	./nexus baz true 4 >> obs.txt

clean:
	rm -rf pool* nexus* __pycache__
