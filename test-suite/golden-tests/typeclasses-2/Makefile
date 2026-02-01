all:
	rm -f obs.txt
	morloc make main.loc 
	./nexus paste '["a","b"]' > obs.txt
	./nexus sum '[1,2,3]' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ y z
