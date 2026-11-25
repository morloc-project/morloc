all:
	rm -f *.err obs.txt
	morloc make --debug -o nexus main.loc 2> build.err
	./nexus applyCpp 5 >> obs.txt 2>> obs.err
	./nexus applyPy 5 >> obs.txt 2>> obs.err
	./nexus totalCpp 1 >> obs.txt 2>> obs.err
	./nexus totalPy 1 >> obs.txt 2>> obs.err
	./nexus cached 2 >> obs.txt 2>> obs.err
	if ./nexus cached 0 > /dev/null 2> zero.err; then echo "UNEXPECTED: cached 0" >> obs.txt; elif grep -q "by zero" zero.err; then echo "OK: cached 0 reports the division" >> obs.txt; else echo "UNEXPECTED: cached 0 error" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ .morloc-debug .morloc-cache
	rm -rf *-build *build.tmp*
	rm -f *.err
