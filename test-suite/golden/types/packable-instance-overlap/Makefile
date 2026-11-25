all:
	rm -f *.err obs.txt a1.txt a2.txt
	if morloc make -o genfirst generic-first.loc 2> build.err; then ./genfirst rootLabel '[["a","b"],[[0,1,1.5]],["x"]]' > a1.txt 2> obs.err; else echo REJECTED > a1.txt; fi
	if morloc make -o specfirst specific-first.loc 2>> build.err; then ./specfirst rootLabel '[["a","b"],[[0,1,1.5]],["x"]]' > a2.txt 2>> obs.err; else echo REJECTED > a2.txt; fi
	if diff -q a1.txt a2.txt > /dev/null; then echo "OK order-independent: $$(cat a1.txt)" >> obs.txt; else echo "UNEXPECTED order-dependent: $$(cat a1.txt) vs $$(cat a2.txt)" >> obs.txt; fi
	if morloc make -o ambiguous ambiguous.loc 2>> build.err; then echo "UNEXPECTED overlap accepted" >> obs.txt; else echo "OK overlap rejected" >> obs.txt; fi
	if morloc make -o atomicambig atomic-ambiguous.loc 2>> build.err; then echo "UNEXPECTED atomic overlap accepted" >> obs.txt; else echo "OK atomic overlap rejected" >> obs.txt; fi
	morloc make -o closure closure.loc 2>> build.err
	./closure rootLabel '[["a","b"],[[0,1,1.5]],["x"]]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err a1.txt a2.txt
	rm -rf genfirst specfirst ambiguous atomicambig closure __pycache__
	rm -rf *-build *build.tmp*
