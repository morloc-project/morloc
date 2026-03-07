all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testPrecedence       > obs.txt 2> obs.err
	./nexus testParens          2>> obs.err  >> obs.txt
	./nexus testNegatives       2>> obs.err  >> obs.txt
	./nexus testNumericLiterals 2>> obs.err  >> obs.txt
	./nexus testMixedArith      2>> obs.err  >> obs.txt
	./nexus testGetterArith     2>> obs.err  >> obs.txt
	./nexus testNestedParens    2>> obs.err  >> obs.txt
	./nexus testAssociativity   2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
