all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus run1 1 >> obs.txt 2>> obs.err
	./nexus run2 1 >> obs.txt 2>> obs.err
	./nexus run3 1 >> obs.txt 2>> obs.err
	./nexus run4 1 >> obs.txt 2>> obs.err
	./nexus run5 1 >> obs.txt 2>> obs.err
	if morloc typecheck -v --realize main.loc > check.err 2> /dev/null && ! grep -q 'cost check' check.err; then echo "OK: built placement costs the least" >> obs.txt; else echo "UNEXPECTED: built placement does not cost the least found" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
