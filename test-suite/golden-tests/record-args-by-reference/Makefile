all:
	rm -f *.err obs.txt
	morloc make -o py py.loc 2> build-py.err
	./py box3 '{"x":{"x":{"x":1,"y":2},"y":{"x":3,"y":4}},"y":{"x":{"x":5,"y":6},"y":{"x":7,"y":8}}}' >> obs.txt 2>> obs.err
	./py boxList '{"x":[{"x":1,"y":2}],"y":[]}' >> obs.txt 2>> obs.err
	./py boxOpt '{"x":{"x":1,"y":2},"y":null}' >> obs.txt 2>> obs.err
	./py optBox '{"Some":[{"x":{"x":1,"y":2},"y":{"x":3,"y":4}}]}' >> obs.txt 2>> obs.err
	./py optBox '"None"' >> obs.txt 2>> obs.err
	morloc make -o cpp cpp.loc 2> build-cpp.err
	./cpp box3 '{"x":{"x":{"x":1,"y":2},"y":{"x":3,"y":4}},"y":{"x":{"x":5,"y":6},"y":{"x":7,"y":8}}}' >> obs.txt 2>> obs.err
	./cpp boxList '{"x":[{"x":1,"y":2}],"y":[]}' >> obs.txt 2>> obs.err
	./cpp boxOpt '{"x":{"x":1,"y":2},"y":null}' >> obs.txt 2>> obs.err
	./cpp optBox '{"Some":[{"x":{"x":1,"y":2},"y":{"x":3,"y":4}}]}' >> obs.txt 2>> obs.err
	./cpp optBox '"None"' >> obs.txt 2>> obs.err
	./cpp tagBox '{"n":5}' >> obs.txt 2>> obs.err
	./cpp holder '{"t":{"n":1},"k":2}' >> obs.txt 2>> obs.err
	./cpp holderBox '{"t":{"n":3},"k":4}' >> obs.txt 2>> obs.err
	morloc make -o rust rust.loc 2> build-rust.err
	./rust box3 '{"x":{"x":{"x":1,"y":2},"y":{"x":3,"y":4}},"y":{"x":{"x":5,"y":6},"y":{"x":7,"y":8}}}' >> obs.txt 2>> obs.err
	./rust boxList '{"x":[{"x":1,"y":2}],"y":[]}' >> obs.txt 2>> obs.err
	./rust boxOpt '{"x":{"x":1,"y":2},"y":null}' >> obs.txt 2>> obs.err
	./rust optBox '{"Some":[{"x":{"x":1,"y":2},"y":{"x":3,"y":4}}]}' >> obs.txt 2>> obs.err
	./rust optBox '"None"' >> obs.txt 2>> obs.err

clean:
	rm -rf py cpp rust pools *err *-build *build.tmp*

