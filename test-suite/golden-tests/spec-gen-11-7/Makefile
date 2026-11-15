all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip '{"pts":[{"x":1},{"x":2}]}' > obs.txt 2> obs.err
	grep '^import' nexus-build/pools/types/py/mlc_types/_local/lib/__init__.py nexus-build/pools/types/py/mlc_types/_local/lib/math/__init__.py >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
