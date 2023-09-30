all:
	morloc typecheck -r main.loc
	morloc make main.loc
	./nexus.py foo '"asdf"' 42 

.PHONY: clean
clean:
	rm -rf pool* nexus* __pycache__
