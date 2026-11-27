all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- exact ---" >> obs.txt
	./nexus rank '"High"' >> obs.txt 2>> obs.err
	./nexus level '{"name":"a","priority":"High"}' >> obs.txt 2>> obs.err
	./nexus ranks '["Low","High"]' >> obs.txt 2>> obs.err
	echo "--- quoted-wrong-case ---" >> obs.txt
	if ./nexus rank '"high"' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- field-wrong-case ---" >> obs.txt
	if ./nexus level '{"name":"a","priority":"high"}' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- list-wrong-case ---" >> obs.txt
	if ./nexus ranks '["low","High"]' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- bare-is-lenient ---" >> obs.txt
	./nexus rank high >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
