all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	echo "--- cpp ---" >> obs.txt
	./nexus-cpp roundChain '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-cpp roundHolder '{"one":{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},"maybe":null,"many":[{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},{"val":5,"next":null}],"tree":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}}' >> obs.txt 2>> obs.err
	./nexus-cpp sumChain '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-cpp countTree '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp holderSum '{"one":{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},"maybe":null,"many":[{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},{"val":5,"next":null}],"tree":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}}' >> obs.txt 2>> obs.err
	./nexus-cpp mkChain 7 >> obs.txt 2>> obs.err
	./nexus-cpp mkHolder '{"val":4,"next":null}' '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp nextOf '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-cpp --keep-null nextOf '{"val":4,"next":null}' >> obs.txt 2>> obs.err
	./nexus-cpp viaPy '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-cpp setNext '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' '{"val":8,"next":null}' >> obs.txt 2>> obs.err
	./nexus-cpp setNextLive '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' '{"val":8,"next":null}' >> obs.txt 2>> obs.err
	./nexus-cpp setVal '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-cpp firstKidNext '{"val":1,"next":null,"kids":[{"val":2,"next":{"val":3,"next":null,"kids":[]},"kids":[]},{"val":4,"next":null,"kids":[]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp kidNexts '{"val":1,"next":null,"kids":[{"val":2,"next":{"val":3,"next":null,"kids":[]},"kids":[]},{"val":4,"next":null,"kids":[]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp roundPairs '{"n":1,"rest":[[5,{"n":2,"rest":[],"opts":[null]}]],"opts":[null,{"n":3,"rest":[],"opts":[]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp mkPairs 7 >> obs.txt 2>> obs.err
	./nexus-cpp wrapLabel '{"root":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]},"tag":"t"}' >> obs.txt 2>> obs.err
	./nexus-cpp treeLabel q >> obs.txt 2>> obs.err
	./nexus-cpp firstKid '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp relabel '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp setInner '{"inner":{"a":"1","b":"2","c":"3"},"next":{"inner":{"a":"4","b":"5","c":"6"},"next":null}}' >> obs.txt 2>> obs.err
	./nexus-cpp grandKidLabel '{"label":"a","kids":[{"label":"b","kids":[{"label":"g","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp kidsOf '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp deepBuild 1000000 >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	echo "--- rust ---" >> obs.txt
	./nexus-rust roundChain '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-rust roundHolder '{"one":{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},"maybe":null,"many":[{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},{"val":5,"next":null}],"tree":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}}' >> obs.txt 2>> obs.err
	./nexus-rust sumChain '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-rust countTree '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust holderSum '{"one":{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},"maybe":null,"many":[{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}},{"val":5,"next":null}],"tree":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}}' >> obs.txt 2>> obs.err
	./nexus-rust mkChain 7 >> obs.txt 2>> obs.err
	./nexus-rust mkHolder '{"val":4,"next":null}' '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust nextOf '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-rust --keep-null nextOf '{"val":4,"next":null}' >> obs.txt 2>> obs.err
	./nexus-rust viaPy '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-rust setNext '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' '{"val":8,"next":null}' >> obs.txt 2>> obs.err
	./nexus-rust setNextLive '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' '{"val":8,"next":null}' >> obs.txt 2>> obs.err
	./nexus-rust setVal '{"val":1,"next":{"val":2,"next":{"val":3,"next":null}}}' >> obs.txt 2>> obs.err
	./nexus-rust firstKidNext '{"val":1,"next":null,"kids":[{"val":2,"next":{"val":3,"next":null,"kids":[]},"kids":[]},{"val":4,"next":null,"kids":[]}]}' >> obs.txt 2>> obs.err
	./nexus-rust kidNexts '{"val":1,"next":null,"kids":[{"val":2,"next":{"val":3,"next":null,"kids":[]},"kids":[]},{"val":4,"next":null,"kids":[]}]}' >> obs.txt 2>> obs.err
	./nexus-rust roundPairs '{"n":1,"rest":[[5,{"n":2,"rest":[],"opts":[null]}]],"opts":[null,{"n":3,"rest":[],"opts":[]}]}' >> obs.txt 2>> obs.err
	./nexus-rust mkPairs 7 >> obs.txt 2>> obs.err
	./nexus-rust wrapLabel '{"root":{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]},"tag":"t"}' >> obs.txt 2>> obs.err
	./nexus-rust treeLabel q >> obs.txt 2>> obs.err
	./nexus-rust firstKid '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust relabel '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust setInner '{"inner":{"a":"1","b":"2","c":"3"},"next":{"inner":{"a":"4","b":"5","c":"6"},"next":null}}' >> obs.txt 2>> obs.err
	./nexus-rust grandKidLabel '{"label":"a","kids":[{"label":"b","kids":[{"label":"g","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust kidsOf '{"label":"a","kids":[{"label":"b","kids":[]},{"label":"c","kids":[{"label":"d","kids":[]}]}]}' >> obs.txt 2>> obs.err
	./nexus-rust deepBuild 1000000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
