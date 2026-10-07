all:
	rm -f *.err obs.txt
	if morloc make -o cpp cpp.loc 2> cpp.err; then echo UNEXPECTED >> obs.txt; elif grep -q "MyBox is used with two field types" cpp.err && ! grep -q CallStack cpp.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if morloc make -o rust rust.loc 2> rust.err; then echo UNEXPECTED >> obs.txt; elif grep -q "MyBox is used with two field types" rust.err && ! grep -q CallStack rust.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -f *.err
	rm -rf cpp rust *-build *build.tmp*
