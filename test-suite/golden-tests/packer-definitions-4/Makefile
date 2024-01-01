all:
	morloc make main.loc
	./nexus.py foo 42 > obs.txt

.PHONY: clean
clean:
	rm -rf pool* nexus* *pdf *rda __pycache__
