all:
	rm -f *.err obs.txt
	morloc typecheck --realize main.loc 2> build.err | sed -n '/^placement:$$/,/^$$/p' > obs.txt
	morloc make -o nexus main.loc 2>> build.err
	./nexus bySignature 1 >> obs.txt
	./nexus byItem 1 >> obs.txt
	./nexus byMethod 1 >> obs.txt
	./nexus slow 1 >> obs.txt
	if morloc typecheck -v --realize main.loc > check.err 2> /dev/null && ! grep -q 'cost check' check.err; then echo "OK: built placement costs the least" >> obs.txt; else echo "UNEXPECTED: built placement does not cost the least found" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
