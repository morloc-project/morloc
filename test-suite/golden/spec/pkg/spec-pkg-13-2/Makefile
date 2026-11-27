all:
	rm -f *.err obs.txt
	mkdir -p fake/lib
	if [ "$$(uname)" = Darwin ]; then cc -dynamiclib -install_name @rpath/libfake.dylib -o fake/lib/libfake.dylib fake.c; else cc -shared -fPIC -o fake/lib/libfake.so fake.c; fi
	env -u MORLOC_STRICT_CONDA MORLOC_ENV=t MORLOC_BUILD_HOOK=true CONDA_PREFIX=$(CURDIR)/fake morloc make -o nexus main.loc 2> build.err
	./nexus answer 1 >> obs.txt 2>> obs.err
	if [ "$$(uname)" = Darwin ]; then otool -l nexus-build/pools/rust/pool-rust.out | grep -c "path $(CURDIR)/fake/lib "; else readelf -d nexus-build/pools/rust/pool-rust.out | grep "(RPATH)" | grep -c "$(CURDIR)/fake/lib"; fi >> obs.txt

clean:
	rm -rf nexus fake *.err
	rm -rf *-build *build.tmp*
