all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err || true
	if grep -q "cannot leave the pool that made it" build.err; then echo "refused at build" >> obs.txt; else echo "UNEXPECTED: not refused" >> obs.txt; fi

clean:
	rm -rf nexus pools *.err __pycache__ .RData
	rm -rf *-build *build.tmp*
