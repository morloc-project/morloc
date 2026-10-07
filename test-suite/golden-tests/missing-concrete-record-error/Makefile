all:
	rm -f *.err obs.txt
	if timeout 30s morloc make -o nexus-main main.loc 2> main.err; then echo UNEXPECTED >> obs.txt; elif grep -q "record py => Container" main.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if timeout 30s morloc make -o nexus-plain plain.loc 2> plain.err; then echo UNEXPECTED >> obs.txt; elif grep -q "record py => Report" plain.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -f *.err
	rm -rf nexus-* *-build *build.tmp*
