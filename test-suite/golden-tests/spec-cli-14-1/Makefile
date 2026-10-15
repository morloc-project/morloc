all:
	rm -f *.err obs.txt swap.csv extra.csv missing.csv dup.csv case.csv empty.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'y,x\nb,2\n' > swap.csv
	printf 'x,y,zip\n1,a,02134\n' > extra.csv
	printf 'x\n1\n' > missing.csv
	printf 'x,y,y\n1,a,b\n' > dup.csv
	printf 'x,Y\n1,a\n' > case.csv
	printf 'x,\n1,a\n' > empty.csv
	./nexus rows swap.csv >> obs.txt 2>> obs.err
	./nexus table swap.csv >> obs.txt 2>> obs.err
	./nexus table extra.csv >> obs.txt 2>> obs.err
	./nexus open extra.csv >> obs.txt 2>> obs.err
	if ./nexus rows extra.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows missing.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus table missing.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows dup.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus open dup.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows case.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus open empty.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ swap.csv extra.csv missing.csv dup.csv case.csv empty.csv
	rm -f *.err
	rm -rf *-build *build.tmp*
