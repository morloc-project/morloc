all:
	rm -f *.err obs.txt ok.csv after.csv open.csv cr.csv blank.csv short.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'x,y\n1,"a, ""b""\nc"\n2,say "hi"\n' > ok.csv
	printf 'x,y\n1,"Best" gene\n' > after.csv
	printf 'x,y\n1,"open\n' > open.csv
	printf 'x,y\n1,a\rb\n' > cr.csv
	printf 'x,y\n1,a\n\n2,b\n' > blank.csv
	printf 'x,y\n1\n' > short.csv
	./nexus rows ok.csv >> obs.txt 2>> obs.err
	if ./nexus rows after.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows open.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows cr.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows blank.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows short.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ ok.csv after.csv open.csv cr.csv blank.csv short.csv
	rm -f *.err
	rm -rf *-build *build.tmp*
