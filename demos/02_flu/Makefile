all: nexus.py
	./nexus.py makeTree data/config.json > tree.json
	./nexus.py plot data/config.json tree.json

nexus.py:
	morloc make main.loc

clean:
	rm -rf pool* nexus* *pdf *rda __pycache__ tree.json
