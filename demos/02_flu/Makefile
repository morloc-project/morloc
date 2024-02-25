all:
	morloc typecheck -r main.loc
	morloc make main.loc
	./nexus.py -h

.PHONY:
search:
	./nexus.py search '{"email":"zbwrnz@gmail.com"}' \
		                '{"email":"zbwrnz@gmail.com","db":"nuccore","mindate":"2023/01/01","maxdate":"2023/01/31","retmax":1000}' \
										'[["KU976624","MG917052"],["1A.3.1","2010-human_like"]]' \
										'"Influenza+A+Virus[Organism]+H3N2[ALL]+HA[ALL]"'

.PHONY:
run:
	./nexus.py analysis config.json

.PHONY:
makec:
	g++ --std=c++17 -o pool-cpp.out pool.cpp -I/home/z/.morloc/include -I/home/z/.morloc/src/morloc/plain/morloclib/bio/tree -I/home/z/.morloc/src/morloc/plain/morloclib/cppbase -I/home/z/.morloc/src/morloc/plain/morloclib/bio/algo -I/home/z/.morloc/src/morloc/plain/morloclib/matrix/eigen

.PHONY: clean
clean:
	rm -rf pool* nexus* *pdf *rda __pycache__
