all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp tripP '{"val":1,"sub":{"val":2,"sub":null,"other":3},"other":4}' >> obs.txt 2>> obs.err
	./nexus-cpp tripQ '{"x":"a","ys":["b","c"],"zs":["d"]}' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust tripP '{"val":1,"sub":{"val":2,"sub":null,"other":3},"other":4}' >> obs.txt 2>> obs.err
	./nexus-rust tripQ '{"x":"a","ys":["b","c"],"zs":["d"]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools __pycache__ *err *-build *build.tmp*
