all:
	rm -f *.err *.dat
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet      big > w3.dat   2>> obs.err
	./nexus -f packet -z 0 big > w3z0.dat 2>> obs.err
	./nexus -f packet -z 5 big > w3z5.dat 2>> obs.err
	./nexus slice w3.dat   >> obs.txt 2>> obs.err
	./nexus slice w3z0.dat >> obs.txt 2>> obs.err
	./nexus slice w3z5.dat >> obs.txt 2>> obs.err
	./nexus count w3.dat   >> obs.txt 2>> obs.err
	if test $$(wc -c < w3.dat) -lt $$(wc -c < w3z0.dat); then echo "OK write level compresses" >> obs.txt; else echo "UNEXPECTED write level ignored" >> obs.txt; fi
	if test $$(wc -c < w3z5.dat) -lt $$(wc -c < w3z0.dat); then echo "OK -z overrides" >> obs.txt; else echo "UNEXPECTED -z ignored" >> obs.txt; fi
	if ./nexus -f packet mixed > mixed.dat 2>> obs.err; then echo "UNEXPECTED mixed levels accepted" >> obs.txt; else echo "OK mixed levels refused" >> obs.txt; fi
	if ./nexus -f packet -z 0 mixed > mixed0.dat 2>> obs.err; then echo "OK -z pins one level" >> obs.txt; else echo "UNEXPECTED mixed levels refused under -z" >> obs.txt; fi
	if ./nexus mixed > /dev/null 2>> obs.err; then echo "OK decoded output ignores the levels" >> obs.txt; else echo "UNEXPECTED mixed levels refused for json" >> obs.txt; fi
	if ./nexus -f packet -z 10 big > /dev/null 2> z10.err; then echo "UNEXPECTED -z 10 accepted" >> obs.txt; else echo "OK -z 10 refused" >> obs.txt; fi
	rm -f *.dat

clean:
	rm -rf nexus pools __pycache__ *.err *.dat
	rm -rf *-build *build.tmp*
