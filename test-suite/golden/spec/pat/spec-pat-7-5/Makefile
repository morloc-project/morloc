all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pickPure '[]' >> obs.txt 2>> obs.err
	./nexus pickPure '[7]' >> obs.txt 2>> obs.err
	./nexus pickPure '[7,8]' >> obs.txt 2>> obs.err
	./nexus pickPure '[7,8,9]' >> obs.txt 2>> obs.err
	./nexus sumPy '[]' >> obs.txt 2>> obs.err
	./nexus sumPy '[5]' >> obs.txt 2>> obs.err
	./nexus sumPy '[0,5]' >> obs.txt 2>> obs.err
	./nexus sumPy '[2,5]' >> obs.txt 2>> obs.err
	./nexus sumPy '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus namePy '[["a",[]]]' >> obs.txt 2>> obs.err
	./nexus namePy '[["a",[1]],["b",[1,2]]]' >> obs.txt 2>> obs.err
	./nexus namePy '[["a",[1,2]],["b",[]]]' >> obs.txt 2>> obs.err
	./nexus namePy '[]' >> obs.txt 2>> obs.err
	./nexus wholePy '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus wholePy '[1,2]' >> obs.txt 2>> obs.err
	./nexus firstPy '["x","y"]' >> obs.txt 2>> obs.err
	./nexus firstPy '["x"]' >> obs.txt 2>> obs.err
	./nexus roundPy '[3,4]' >> obs.txt 2>> obs.err
	./nexus roundPy '[0]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[5]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[0,5]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[2,5]' >> obs.txt 2>> obs.err
	./nexus sumCpp '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus nameCpp '[["a",[]]]' >> obs.txt 2>> obs.err
	./nexus nameCpp '[["a",[1]],["b",[1,2]]]' >> obs.txt 2>> obs.err
	./nexus nameCpp '[["a",[1,2]],["b",[]]]' >> obs.txt 2>> obs.err
	./nexus nameCpp '[]' >> obs.txt 2>> obs.err
	./nexus wholeCpp '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus wholeCpp '[1,2]' >> obs.txt 2>> obs.err
	./nexus firstCpp '["x","y"]' >> obs.txt 2>> obs.err
	./nexus firstCpp '["x"]' >> obs.txt 2>> obs.err
	./nexus roundCpp '[3,4]' >> obs.txt 2>> obs.err
	./nexus roundCpp '[0]' >> obs.txt 2>> obs.err
	./nexus sumR '[]' >> obs.txt 2>> obs.err
	./nexus sumR '[5]' >> obs.txt 2>> obs.err
	./nexus sumR '[0,5]' >> obs.txt 2>> obs.err
	./nexus sumR '[2,5]' >> obs.txt 2>> obs.err
	./nexus sumR '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus nameR '[["a",[]]]' >> obs.txt 2>> obs.err
	./nexus nameR '[["a",[1]],["b",[1,2]]]' >> obs.txt 2>> obs.err
	./nexus nameR '[["a",[1,2]],["b",[]]]' >> obs.txt 2>> obs.err
	./nexus nameR '[]' >> obs.txt 2>> obs.err
	./nexus wholeR '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus wholeR '[1,2]' >> obs.txt 2>> obs.err
	./nexus firstR '["x","y"]' >> obs.txt 2>> obs.err
	./nexus firstR '["x"]' >> obs.txt 2>> obs.err
	./nexus roundR '[3,4]' >> obs.txt 2>> obs.err
	./nexus roundR '[0]' >> obs.txt 2>> obs.err
	./nexus sumRust '[]' >> obs.txt 2>> obs.err
	./nexus sumRust '[5]' >> obs.txt 2>> obs.err
	./nexus sumRust '[0,5]' >> obs.txt 2>> obs.err
	./nexus sumRust '[2,5]' >> obs.txt 2>> obs.err
	./nexus sumRust '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus nameRust '[["a",[]]]' >> obs.txt 2>> obs.err
	./nexus nameRust '[["a",[1]],["b",[1,2]]]' >> obs.txt 2>> obs.err
	./nexus nameRust '[["a",[1,2]],["b",[]]]' >> obs.txt 2>> obs.err
	./nexus nameRust '[]' >> obs.txt 2>> obs.err
	./nexus wholeRust '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus wholeRust '[1,2]' >> obs.txt 2>> obs.err
	./nexus firstRust '["x","y"]' >> obs.txt 2>> obs.err
	./nexus firstRust '["x"]' >> obs.txt 2>> obs.err
	./nexus roundRust '[3,4]' >> obs.txt 2>> obs.err
	./nexus roundRust '[0]' >> obs.txt 2>> obs.err
	./nexus bindPy 2 >> obs.txt 2>> obs.err
	if ./nexus bindPy 3 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	./nexus bindCpp 2 >> obs.txt 2>> obs.err
	if ./nexus bindCpp 3 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	./nexus bindR 2 >> obs.txt 2>> obs.err
	if ./nexus bindR 3 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	./nexus bindRust 2 >> obs.txt 2>> obs.err
	if ./nexus bindRust 3 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
