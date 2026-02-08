all:
	rm -f obs.txt
	morloc make main.loc
	./nexus testStep --seed 42 > obs.txt
	./nexus play --seed 12345 --time 0.0 --steps 1 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
