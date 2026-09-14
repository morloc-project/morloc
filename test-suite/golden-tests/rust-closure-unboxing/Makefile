all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus viaMap 1000 >> obs.txt 2>> obs.err
	grep -c 'dyn rustmorloc::MorlocFn' nexus-build/pools/rust/src/main.rs >> obs.txt || true
	grep -c 'std::rc::Rc::new' nexus-build/pools/rust/src/main.rs >> obs.txt || true

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
