all:
	morloc make foo.loc
	./nexus.py length '"yolo my fomo"' > obs.txt
	./nexus.py upper '"yolo my fomo"' >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
