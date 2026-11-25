all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err || true
	if grep -q "needs Tag Int" build.err; then echo "OK: context named" >> obs.txt; else echo "UNEXPECTED: context not named" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
