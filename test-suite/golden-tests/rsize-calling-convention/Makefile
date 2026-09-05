all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus scaled 3 '[1,2]' 2>> obs.err >> obs.txt
	./nexus shifted 10 '[1,2]' 2>> obs.err >> obs.txt
	./nexus doubler '[1,2]' 2>> obs.err >> obs.txt
	./nexus ranged 1 10 5 2>> obs.err >> obs.txt
	./nexus ranged 1 10 50 2>> obs.err >> obs.txt
	./nexus clamped 0 10 42 2>> obs.err >> obs.txt
	./nexus added 2 3 2>> obs.err >> obs.txt
	grep -c 'lib.scale(.*) (' nexus-build/pools/py/pool.py >> obs.txt
	grep -c 'lib.shift(.*, .*)' nexus-build/pools/py/pool.py >> obs.txt
	grep -c 'lib.native_name_add' nexus-build/pools/py/pool.py >> obs.txt
	morloc make -o bad bad-zero.loc 2> obs.zero.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo zero_rejected >> obs.txt
	morloc make -o bad bad-negative.loc 2> obs.negative.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo negative_rejected >> obs.txt
	morloc make -o bad bad-noninteger.loc 2> obs.noninteger.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo noninteger_rejected >> obs.txt
	morloc make -o bad bad-toolarge.loc 2> obs.toolarge.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo toolarge_rejected >> obs.txt
	morloc make -o bad bad-exact.loc 2> obs.exact.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo exact_rejected >> obs.txt

clean:
	rm -rf nexus bad __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
