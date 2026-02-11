all:
	rm -f obs.txt
	morloc make main.loc 2> build.err
	./nexus testStep --seed 42 > obs.txt 2> obs.err
	./nexus play --seed 12345 --time 0.0 --steps 1 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
