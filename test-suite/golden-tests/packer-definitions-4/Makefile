all:
	morloc make main.loc
	./nexus.py foo '"Alice"' > obs.txt

.PHONY: clean
clean:
	rm -rf pool* nexus* *pdf *rda __pycache__
