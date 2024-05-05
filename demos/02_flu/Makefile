all:
	morloc typecheck -r main.loc
	morloc make main.loc
	./nexus.py -h

.PHONY:
search:
	./nexus.py searchEntrez config.json > searchResult.json

.PHONY:
run:
	./nexus.py classifyByDate config.json > tree.json
	./nexus.py plotCladeTree config.json tree.json

.PHONY:
makec:
	g++ --std=c++17 -o pool-cpp.out pool.cpp -I/home/z/.morloc/include -I/home/z/.morloc/src/morloc/plane/morloclib/bio/tree -I/home/z/.morloc/src/morloc/plane/morloclib/cppbase -I/home/z/.morloc/src/morloc/plane/morloclib/bio/algo -I/home/z/.morloc/src/morloc/plane/morloclib/matrix/eigen

.PHONY: clean
clean:
	rm -rf pool* nexus* *pdf *rda __pycache__
