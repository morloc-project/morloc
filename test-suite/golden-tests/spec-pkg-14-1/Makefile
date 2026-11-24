all:
	rm -f *.err obs.txt
	env -u MORLOC_ENV -u MORLOC_BUILD_HOOK -u MORLOC_STRICT_CONDA morloc make -o nexus main.loc 2> build.err && echo UNEXPECTED_SUCCESS > obs.txt || (grep -q "C++ dependency no_such_dep (header no_such_dep/x.hpp) not found" build.err && echo REFUSED) > obs.txt

clean:
	rm -rf nexus *.err
	rm -rf *-build *build.tmp*
