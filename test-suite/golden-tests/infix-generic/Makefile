all:
	morloc make main.loc 
	./nexus x > obs.txt
	./nexus y >> obs.txt
	./nexus z >> obs.txt
	./nexus f 99 >> obs.txt

clean:
	rm -rf nexus* pool* log __pycache__
