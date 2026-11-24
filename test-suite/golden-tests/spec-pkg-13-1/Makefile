all:
	rm -f *.err obs.txt
	mkdir -p fake/lib bad/lib && touch bad/lib/libmorloc.so
	if [ "$$(uname)" = Darwin ]; then cc -dynamiclib -install_name @rpath/libfake.dylib -o fake/lib/libfake.dylib fake.c; else cc -shared -fPIC -o fake/lib/libfake.so fake.c; fi
	env -u MORLOC_STRICT_CONDA MORLOC_ENV=t MORLOC_BUILD_HOOK=true CONDA_PREFIX=$(CURDIR)/fake morloc make -o nexus main.loc 2> build.err
	./nexus answer 1 >> obs.txt 2>> obs.err
	if [ "$$(uname)" = Darwin ]; then otool -l nexus-build/pools/cpp/pool-cpp.out | grep -c "path $(CURDIR)/fake/lib "; else readelf -d nexus-build/pools/cpp/pool-cpp.out | grep "(RPATH)" | grep -c "$(CURDIR)/fake/lib"; fi >> obs.txt
	env -u MORLOC_STRICT_CONDA MORLOC_ENV=t MORLOC_BUILD_HOOK=true CONDA_PREFIX=$(CURDIR)/missing morloc make -o nexus2 main.loc 2> missing.err && echo UNEXPECTED_SUCCESS >> obs.txt || (grep -q "not a directory" missing.err && echo REFUSED) >> obs.txt
	env -u MORLOC_STRICT_CONDA MORLOC_ENV=t MORLOC_BUILD_HOOK=true CONDA_PREFIX=$(CURDIR)/bad morloc make -o nexus3 main.loc 2> bad.err && echo UNEXPECTED_SUCCESS >> obs.txt || (grep -q "holds a libmorloc" bad.err && echo REFUSED) >> obs.txt

clean:
	rm -rf nexus nexus2 nexus3 *.err fake/lib bad
	rm -rf *-build *build.tmp*
