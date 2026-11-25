all:
	rm -f *.err obs.txt
	if timeout 300s morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err; then echo UNEXPECTED >> obs.txt; \
	elif grep -q 'is used at two morloc types' build-cpp.err; then echo OK >> obs.txt; \
	else echo UNEXPECTED >> obs.txt; fi
	if timeout 300s morloc make -o nexus-rust main-rust.loc 2> build-rust.err; then echo UNEXPECTED >> obs.txt; \
	elif grep -q 'is used at two morloc types' build-rust.err; then echo OK >> obs.txt; \
	else echo UNEXPECTED >> obs.txt; fi
	if timeout 300s morloc make -o nexus-closures main-rust-closures.loc 2> build-closures.err; then echo UNEXPECTED >> obs.txt; \
	elif grep -q 'holds a function inside a container' build-closures.err; then echo OK >> obs.txt; \
	else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus-cpp nexus-rust nexus-closures pools __pycache__ *err *-build *build.tmp*
