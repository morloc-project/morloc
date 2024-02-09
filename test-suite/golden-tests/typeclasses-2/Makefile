all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.py paste '["a","b"]' > obs.txt
	./nexus.py sum '[1,2,3]' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ y z
