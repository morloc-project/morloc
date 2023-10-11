all:
	morloc typecheck -r main.loc
	morloc make main.loc
	./nexus.py -h

.PHONY:
run:
	./nexus.py analysis '"2021-01-01"' '"2021-12-31"' '"refs.txt"' '"tree.pdf"'

.PHONY: clean
clean:
	rm -rf pool* nexus* __pycache__
