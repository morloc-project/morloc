all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	if grep -q "nosuchdirective" build.err; then echo "OK: unknown argument directive warned" > obs.txt; else echo "MISSING: unknown argument directive" > obs.txt; fi
	if grep -q "metvar" build.err; then echo "OK: near-miss argument directive warned" >> obs.txt; else echo "MISSING: near-miss argument directive" >> obs.txt; fi
	if grep -q "rsizee" build.err; then echo "OK: unknown source directive warned" >> obs.txt; else echo "MISSING: unknown source directive" >> obs.txt; fi
	if grep -q "notadirective" build.err; then echo "UNEXPECTED: escaped prose warned" >> obs.txt; else echo "OK: escaped prose silent" >> obs.txt; fi
	./nexus f 3 '[1,2]' >> obs.txt 2>> obs.err
	./nexus g 7 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
