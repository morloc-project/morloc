all:
	morloc make main.loc 2
	./nexus foo 14 > obs.txt
	./nexus foo2 2 >> obs.txt
	./nexus foo3 3 >> obs.txt

clean:
	rm -rf pool* nexus* __pycache__
